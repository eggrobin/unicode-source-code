with Unicode.Character_Database;

package body Unicode.Properties is
   
   type Binary_Property_Values is array (Code_Point) of Boolean;
   
   type Binary_Property_Data is
      record
         Values : Binary_Property_Values := (others => False);
         Set    : Code_Point_Set;
      end record;
   
   Binary_Properties : array (Binary_Property) of Binary_Property_Data;
   
   procedure Process_Binary_Property_Field 
     (Scope  : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
      Number : Unicode.Character_Database.Field_Number;
      Field  : Wide_Wide_String) is
      Property : constant Binary_Property :=
                   Binary_Property'Wide_Wide_Value (Field);
      Set      : Code_Point_Set := To_Set (Scope);
      use type Unicode.Character_Database.Field_Number;
   begin       
      if Number /= 1 then
         raise Constraint_Error with "Unexpected field " & Number'Image;
      end if;
      Binary_Properties (Property).Set :=
        Binary_Properties (Property).Set or Set;
      for C in Scope.Low .. Scope.High loop
         Binary_Properties (Property).Values (C) := True;
      end loop;
   end Process_Binary_Property_Field;
   
   function Get (Property : Binary_Property; C : Code_Point) return Boolean is
     (Binary_Properties (Property).Values (C));
   
   function Set_Of (Property : Binary_Property) return Code_Point_Set is
     (Binary_Properties (Property).Set);
   
   function Parse_Bidi_Class (Value : Wide_Wide_String) return Bidi_Class is
      type Bidi_Class_Alias is 
        (L, R, AL,
         EN, ES, ET, AN, CS, NSM, BN,
         B, S, WS, ON,
         LRE, LRO, RLE, RLO, PDF,
         LRI, RLI, FSI, PDI);
   begin
      return Bidi_Class'Wide_Wide_Value (Value);
   exception
      when Constraint_Error =>
         return Bidi_Class'Val (Bidi_Class_Alias'Pos 
                                (Bidi_Class_Alias'Wide_Wide_Value (Value)));
   end Parse_Bidi_Class;
   
   function Parse_General_Category (Value : Wide_Wide_String)
                                    return General_CAtegory is
      type General_Category_Alias is 
        (Lu, Ll, Lt, Lm, Lo,
         Mn, Mc, Me,
         Nd, Nl, No,
         Pc, Pd, Ps, Pe, Pi, Pf, Po,
         Sm, Sc, Sk, So,
         Zs, Zl, Zp,
         Cc, Cf, Cs, Co, Cn);
   begin
      return General_Category'Wide_Wide_Value (Value);
   exception
      when Constraint_Error =>
         return General_Category'Val 
           (General_Category_Alias'Pos 
              (General_Category_Alias'Wide_Wide_Value (Value)));
   end Parse_General_Category;
   
   generic
      type Property is (<>);
      File_Name : String;
      with function Parse_Property (S : Wide_Wide_String) return Property;
   package Enumeration_Properties is
      Values : array (Code_Point) of Property;
      Sets   : array (Property) of Code_Point_Set;

      function Value (C : Code_Point) return Property is (Values (C));
      
      function Set_Of (P : Property) return Code_Point_Set is (Sets (P));      
   end Enumeration_Properties;
   
   package body Enumeration_Properties is
      procedure Process_Field 
        (Scope  : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
         Number : Unicode.Character_Database.Field_Number;
         Field  : Wide_Wide_String) is
         New_Value : constant Property := Parse_Property (Field);
         Set       : Code_Point_Set := To_Set (Scope);
         use type Unicode.Character_Database.Field_Number;
      begin       
         if Number /= 1 then
            raise Constraint_Error with "Unexpected field " & Number'Image;
         end if;
         for Value in Property loop
            Sets (Value) := Sets (Value) - Set;
         end loop;
         Sets (New_Value) := Sets (New_Value) or Set;
         for C in Scope.Low .. Scope.High loop
            Values (C) := New_Value;
         end loop;
      end Process_Field;
      
      procedure Process_File is
        new Unicode.Character_Database.Process_File (Process_Field);
   begin
      Process_File (File_Name);
   end Enumeration_Properties;
   
   package Bidi_Classes is new Enumeration_Properties
     (Bidi_Class,
      "DerivedBidiClass.txt",
      Parse_Bidi_Class);
   
   package General_Categories is new Enumeration_Properties
     (General_Category,
      "DerivedGeneralCategory.txt",
      Parse_General_Category);
   
   procedure Process_Binary_Property_File is
     new Unicode.Character_Database.Process_File
       (Process_Binary_Property_Field);

   function Get_General_Category (C : Code_Point) return General_Category 
                                  renames General_Categories.Value;
   
   function Set_Of (Property : General_Category) return Code_Point_Set
                    renames General_Categories.Set_Of;

   function Get_Bidi_Class (C : Code_Point) return Bidi_Class
                            renames Bidi_Classes.Value;      
   function Set_Of (Property : Bidi_Class) return Code_Point_Set
                    renames Bidi_Classes.Set_Of;
   
begin
 
   Process_Binary_Property_File ("DerivedCoreProperties.txt");
   Process_Binary_Property_File ("PropList.txt");
   Process_Binary_Property_File ("DerivedBinaryProperties.txt");
   
end Unicode.Properties;
