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
   
   type Simple_Mapping is array (Code_Point) of Code_Point;
   
   function Identity return Simple_Mapping is
      Result : Simple_Mapping;
   begin
      for C in Code_Point loop
         Result (C) := C;
      end loop;
      return Result;
   end Identity;

   -- TODO (egg): In Ada 2022, initialize these with comprehensions.
   Simple_Lowercase_Mapping : Simple_Mapping := Identity;
   Simple_Titlecase_Mapping : Simple_Mapping := Identity;
   Simple_Uppercase_Mapping : Simple_Mapping := Identity;
   
   procedure Process_Simple_Case_Mapping_Field
     (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Field_Number;
      Field  : Wide_Wide_String) is
      subtype Simple_Case_Mapping_Field is
        Unicode.Character_Database.Field_Number range 12 .. 14;
   begin
      if Number in Simple_Case_Mapping_Field and then Field /= "" then
         declare
            Value : constant Code_Point :=
              Unicode.Character_Database.Parse_Code_Point (Field);
         begin
            for C in Scope.Low .. Scope.High loop
               case Simple_Case_Mapping_Field'(Number) is
                  when 12 => Simple_Uppercase_Mapping (C) := Value;
                  when 13 => Simple_Lowercase_Mapping (C) := Value;
                  when 14 => Simple_Titlecase_Mapping (C) := Value;
               end case;   
            end loop;
         end;
      end if;
   end Process_Simple_Case_Mapping_Field;
   
   procedure Process_Simple_Case_Mappings is
     new Unicode.Character_Database.Process_File
       (Process_Simple_Case_Mapping_Field);
   
   type Special_Mapping_Value is access Wide_Wide_String;
   type Special_Mapping is array (Code_Point) of Special_Mapping_Value;
   
   type Special_Casing is
      record
         Scope : Code_Point_Range;
         Lower : Special_Mapping_Value;
         Title : Special_Mapping_Value;
         Upper : Special_Mapping_Value;
      end record;
   
   Special_Lowercase_Mapping : Special_Mapping := (others => null);
   Special_Titlecase_Mapping : Special_Mapping := (others => null);
   Special_Uppercase_Mapping : Special_Mapping := (others => null);
   
   Current_Record : Special_Casing := (('Z', 'A'), null, null, null);
   
   procedure Process_Special_Casing_Field
     (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Field_Number;
      Field  : Wide_Wide_String) is
   begin
      if Scope /= Current_Record.Scope then
         for C in Scope.High .. Scope.Low loop
            Special_Lowercase_Mapping (C) := Current_Record.Lower;
            Special_Titlecase_Mapping (C) := Current_Record.Title;
            Special_Uppercase_Mapping (C) := Current_Record.Upper;
         end loop;
         Current_Record := (Scope, null, null, null);
      end if;
      case Number is
         when 1 .. 3 =>
            declare
               Value           : Wide_Wide_String :=
                 Unicode.Character_Database.Parse_Sequence (Field);
               Allocated_Value : Special_Mapping_Value :=
                 (if Value = "" then null
                  else new Wide_Wide_String'(Value));
            begin
               case Number is
                  when 1 => Current_Record.Lower := Allocated_Value; 
                  when 2 => Current_Record.Lower := Allocated_Value;
                  when 3 => Current_Record.Lower := Allocated_Value;
                  when others => null;
               end case;
            end;
         when 4 =>
            if Field /= "" then
               -- There is a condition.  We only look at the default case
               -- mappings, so we ignore this record by emptying the range.
               Current_Record.Scope := ('Z', 'A');
            end if;
         when 5 => null;
         when others => raise Constraint_Error;
      end case;
   end Process_Special_Casing_Field;
   
   procedure Process_Special_Casing is
     new Unicode.Character_Database.Process_File (Process_Special_Casing_Field);

begin
 
   Process_Simple_Case_Mappings ("UnicodeData.txt");
   Process_Special_Casing ("SpecialCasing.txt");

   Process_Binary_Property_File ("DerivedCoreProperties.txt");
   Process_Binary_Property_File ("PropList.txt");
   Process_Binary_Property_File ("DerivedBinaryProperties.txt");
   
end Unicode.Properties;
