with Ada.Directories;
with Unicode.Character_Database.Parser;

package body Unicode.Character_Database is

   function Get (UCD: Database; Property : Binary_Property; C : Code_Point) return Boolean is
   (UCD.Binary_Properties (Property).Values (C));
   
   function Set_Of (UCD: Database; Property : Binary_Property) return Code_Point_Set is
   (UCD.Binary_Properties (Property).Set);

   function Get_General_Category (UCD: Database; C : Code_Point) return General_Category
   is (UCD.General_Categories.Values (C));
   function Set_Of (UCD: Database; Property : General_Category) return Code_Point_Set
   is (UCD.General_Categories.Sets (Property));

   function Get_Bidi_Class (UCD: Database; C : Code_Point) return Bidi_Class
   is (UCD.Bidi_Classes.Values (C));
   function Set_Of (UCD: Database; Property : Bidi_Class) return Code_Point_Set
   is (UCD.Bidi_Classes.Sets (Property));
      
   function Set_Of (UCD: Database; Property : Line_Break) return Code_Point_Set
   is (UCD.Line_Breaking_Classes.Sets (Property));
      
   function Set_Of (UCD: Database; Property : Script) return Code_Point_Set
   is (UCD.Scripts.Sets (Property));
      
   function Set_Of (UCD: Database; Property : Decomposition_Type) return Code_Point_Set
   is (UCD.Decomposition_Types.Sets (Property));   
      
   function Set_Of (UCD: Database; Property : East_Asian_Width) return Code_Point_Set
   is (UCD.East_Asian_Width_Classes.Sets (Property));   

   function Get_Canonical_Combining_Class (UCD: Database;
                                           C : Code_Point)
                                           return Canonical_Combining_Class is
   begin
      raise Program_Error;
      return 0;
   end Get_Canonical_Combining_Class;

   function Lowercase_Mapping (UCD : Database; C : Code_Point) return Wide_Wide_String is
   (if UCD.Special_Lowercase_Mapping (C) /= null
      then UCD.Special_Lowercase_Mapping (C).all
      else (1 => UCD.Simple_Lowercase_Mapping (C)));
   function Titlecase_Mapping (UCD : Database; C : Code_Point) return Wide_Wide_String is
   (if UCD.Special_Titlecase_Mapping (C) /= null
      then UCD.Special_Titlecase_Mapping (C).all
      else (1 => UCD.Simple_Titlecase_Mapping (C)));
   function Uppercase_Mapping (UCD : Database; C : Code_Point) return Wide_Wide_String is
   (if UCD.Special_Uppercase_Mapping (C) /= null
      then UCD.Special_Uppercase_Mapping (C).all
      else (1 => UCD.Simple_Uppercase_Mapping (C)));

   function Case_Folding (UCD : Database; C : Code_Point) return Wide_Wide_String is
   (if UCD.Full_Case_Folding (C) = null then (1 => UCD.Simple_Case_Folding (C))
      else UCD.Full_Case_Folding (C).all);

   function Simple_Case_Folding (UCD : Database; C : Code_Point) return Code_Point is
   (UCD.Simple_Case_Folding (C));
   
   function NFKC_Casefold (UCD: Database; C : Code_Point) return Wide_Wide_String is
   (if UCD.Nontrivial_NFKC_Casefold (C) = null then (1 => C)
      else UCD.Nontrivial_NFKC_Casefold (C).all);
   
   function Canonical_Decomposition (UCD : Database; C : Code_Point) return Wide_Wide_String is
   begin
      if UCD.Canonical_Mapping (C) = null then
         return (1 => C);
      else
         declare
            Mapping : Wide_Wide_String renames UCD.Canonical_Mapping (C).all;
         begin
            -- Stability policy:
            -- Canonical mappings (Decomposition_Mapping property values) are
            -- always limited either to a single value or to a pair. The second
            -- character in the pair cannot itself have a canonical mapping.
            case Mapping'Length is
               when 1 =>
                  return UCD.Canonical_Decomposition (Mapping (Mapping'First));
               when 2 =>
                  if UCD.Canonical_Mapping (Mapping (Mapping'Last)) /= null then
                     raise Constraint_Error;
                  end if;
                  return UCD.Canonical_Decomposition (Mapping (Mapping'First)) &
                  Mapping (Mapping'Last);
               when others => raise Constraint_Error;
            end case;
         end;
      end if;         
   end Canonical_Decomposition;   

   function Read_UCD (Directory : String) return Access_Database is
   UCD : Access_Database := new Database;
   procedure Process_Binary_Property_Field 
   (  Scope  : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
      Property : constant Binary_Property :=
         (if Field = "STerm" then Sentence_Terminal else
          Binary_Property'Wide_Wide_Value (Field));
      Set      : Code_Point_Set := To_Set (Scope);
      use type Unicode.Character_Database.Parser.Field_Number;
   begin       
      if Number /= 1 then
         raise Constraint_Error with "Unexpected field " & Number'Image;
      end if;
      UCD.Binary_Properties (Property).Set :=
      UCD.Binary_Properties (Property).Set or Set;
      for C in Scope.Low .. Scope.High loop
         UCD.Binary_Properties (Property).Values (C) := True;
      end loop;
   end Process_Binary_Property_Field;
   
   
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
                                    return General_Category is
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
   
   function Parse_Line_Break (Value : Wide_Wide_String)
                                    return Line_Break is
   begin
      for Candidate in Line_Break loop
         if Line_Break_Aliases (Candidate).all = Value then
            return Candidate;
         end if;
      end loop;
      raise Constraint_Error;
   end Parse_Line_Break;
   
   function Parse_Script (Value : Wide_Wide_String)
                                    return Script is
   begin
      for Candidate in Script loop
         if Script_Aliases (Candidate).all = Value then
            return Candidate;
         end if;
      end loop;
      raise Constraint_Error;
   end Parse_Script;
   
   function Parse_East_Asian_Width (Value : Wide_Wide_String)
                                    return East_Asian_Width is
      type East_Asian_Width_Alias is (A,
                                    F,
                                    H,
                                    N,
                                    Na,
                                    W);
   begin
      return East_Asian_Width'Wide_Wide_Value (Value);
   exception
      when Constraint_Error =>
         return East_Asian_Width'Val 
         (East_Asian_Width_Alias'Pos 
            (East_Asian_Width_Alias'Wide_Wide_Value (Value)));
   end Parse_East_Asian_Width;
      
   generic
      type Property is (<>);
      File_Name : String;
      with function Parse_Property (Value : Wide_Wide_String) return Property;
      with package Property_Data is new Enumeration_Property_Data (Property);
   procedure Process_Enumeration_Property (Data : out Property_Data.Data);
   procedure Process_Enumeration_Property (Data : out Property_Data.Data) is
      procedure Process_Field
        (Scope  : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
         Number : Unicode.Character_Database.Parser.Field_Number;
         Field  : Wide_Wide_String)
      is
         New_Value : constant Property := Parse_Property (Field);
         Set       : Code_Point_Set    := To_Set (Scope);
         use type Unicode.Character_Database.Parser.Field_Number;
      begin
         if Number /= 1 then
            raise Constraint_Error with "Unexpected field " & Number'Image;
         end if;
         for Value in Property loop
            Data.Sets (Value) := Data.Sets (Value) - Set;
         end loop;
         Data.Sets (New_Value) := Data.Sets (New_Value) or Set;
         for C in Scope.Low .. Scope.High loop
            Data.Values (C) := New_Value;
         end loop;
      end Process_Field;
      
      procedure Process_File is
      new Unicode.Character_Database.Parser.Process_File (Process_Field);
   begin
      Process_File (File_Name);
   end Process_Enumeration_Property;

   Extracted : constant String := Ada.Directories.Compose(Directory, "extracted");
   
   procedure Process_Bidi_Classes is new Process_Enumeration_Property
   (Bidi_Class,
      Ada.Directories.Compose(Extracted, "DerivedBidiClass", "txt"),
      Parse_Bidi_Class,
      Bidi_Class_Data);
   
   procedure Process_General_Categories is new Process_Enumeration_Property
   (General_Category,
      Ada.Directories.Compose(Extracted, "DerivedGeneralCategory", "txt"),
      Parse_General_Category,
      General_Category_Data);
   
   procedure Process_Line_Breaking_Classes is new Process_Enumeration_Property
   (Line_Break,
      Ada.Directories.Compose(Directory, "LineBreak", "txt"),
      Parse_Line_Break,
      Line_Break_Data);
   
   procedure Process_Scripts is new Process_Enumeration_Property
   (Script,
      Ada.Directories.Compose(Directory, "Scripts", "txt"),
      Script'Wide_Wide_Value,
      Script_Data);
   
   procedure Process_Decomposition_Types is new Process_Enumeration_Property
   (Decomposition_Type,
      Ada.Directories.Compose(Extracted, "DerivedDecompositionType", "txt"),
      Decomposition_Type'Wide_Wide_Value,
      Decomposition_Type_Data);
   
   procedure Process_East_Asian_Widths is new Process_Enumeration_Property
   (East_Asian_Width,
      Ada.Directories.Compose(Extracted, "DerivedEastAsianWidth", "txt"),
      Parse_East_Asian_Width,
      East_Asian_Width_Data);
   
   procedure Process_Binary_Property_File is
   new Unicode.Character_Database.Parser.Process_File
      (Process_Binary_Property_Field);

   function Identity return Simple_Mapping is
      Result : Simple_Mapping;
   begin
      for C in Code_Point loop
         Result (C) := C;
      end loop;
      return Result;
   end Identity;
   
   procedure Process_Simple_Case_Mapping_Field
   (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
      subtype Simple_Case_Mapping_Field is
      Unicode.Character_Database.Parser.Field_Number range 12 .. 14;
   begin
      if Number in Simple_Case_Mapping_Field and then Field /= "" then
         declare
            Value : constant Code_Point :=
            Parser.Parse_Code_Point (Field);
         begin
            for C in Scope.Low .. Scope.High loop
               case Simple_Case_Mapping_Field'(Number) is
                  when 12 => UCD.Simple_Uppercase_Mapping (C) := Value;
                  when 13 => UCD.Simple_Lowercase_Mapping (C) := Value;
                  when 14 => UCD.Simple_Titlecase_Mapping (C) := Value;
               end case;   
            end loop;
         end;
      end if;
   end Process_Simple_Case_Mapping_Field;
   
   procedure Process_Simple_Case_Mappings is
   new Unicode.Character_Database.Parser.Process_File
      (Process_Simple_Case_Mapping_Field);
      
   procedure Process_Decomposition_Mapping_Field
   (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
      use type Unicode.Character_Database.Parser.Field_Number;
   begin
      if Number = 5 and then Field /= "" and then
      not (for some C of Field => C = '<')  then
         for C in Scope.Low .. Scope.High loop
            UCD.Canonical_Mapping (C) :=
            new Wide_Wide_String'
               (Parser.Parse_Sequence (Field));
         end loop;
      end if;
   end Process_Decomposition_Mapping_Field;
   
   procedure Process_Decomposition_Mappings is
   new Unicode.Character_Database.Parser.Process_File
      (Process_Decomposition_Mapping_Field);
   
   type Special_Casing is
      record
         Scope : Code_Point_Range;
         Lower : Special_Mapping_Value;
         Title : Special_Mapping_Value;
         Upper : Special_Mapping_Value;
      end record;
   
   Current_SC_Record : Special_Casing := (('Z', 'A'), null, null, null);
   
   procedure Process_Special_Casing_Field
   (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
   begin
      if Scope /= Current_SC_Record.Scope then
         Current_SC_Record := (Scope, null, null, null);
      end if;
      case Number is
         when 1 .. 3 =>
            declare
               Value           : Wide_Wide_String :=
                  (if Field = "<slc>" or Field = "<stc>" or Field = "<suc>" then
                  "" else Parser.Parse_Sequence (Field));
               Allocated_Value : Special_Mapping_Value :=
               (if Value = "" then null
                  else new Wide_Wide_String'(Value));
            begin
               case Number is
                  when 1 => Current_SC_Record.Lower := Allocated_Value; 
                  when 2 => Current_SC_Record.Title := Allocated_Value;
                  when 3 => Current_SC_Record.Upper := Allocated_Value;
                  when others => null;
               end case;
            end;
         when 4 =>
            if Field = "" then
               for C in Scope.Low .. Scope.High loop
                  UCD.Special_Lowercase_Mapping (C) := Current_SC_Record.Lower;
                  UCD.Special_Titlecase_Mapping (C) := Current_SC_Record.Title;
                  UCD.Special_Uppercase_Mapping (C) := Current_SC_Record.Upper;
               end loop;
            end if;
         when 5 =>
            -- There was a condition (field 4).  We only look at the default
            -- case mappings, so we ignore this record.
            if Field /= "" then
               raise Constraint_Error;
            end if;
         when others => raise Constraint_Error;
      end case;
   end Process_Special_Casing_Field;
   
   procedure Process_Special_Casing is
   new Parser.Process_File (Process_Special_Casing_Field);
   
   type Case_Folding_Status is (C, F, S, T);
   function Common return Case_Folding_Status renames C;
   function Full return Case_Folding_Status renames F;
   function Simple return Case_Folding_Status renames S;
   function Turkic return Case_Folding_Status renames T;
   
   type Case_Folding_Record is
      record
         Scope  : Code_Point_Range;
         Status : Case_Folding_Status;
      end record;
   
   Current_CF_Record : Case_Folding_Record := (('Z', 'A'), C);
   
   procedure Process_Case_Folding_Field
   (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
   begin
      if Scope /= Current_CF_Record.Scope then
         Current_CF_Record := (Scope, C);
      end if;
      case Number is
         when 1 =>
            Current_CF_Record.Status :=
            Case_Folding_Status'Wide_Wide_Value (Field);
         when 2 =>
            if Field = "<code point>" then
               -- Some versions of Unicode had an @missing line in CaseFolding.txt.
               return;
            end if;
            for C in Scope.Low .. Scope.High loop
               case Current_CF_Record.Status is
                  when Common =>
                     UCD.Simple_Case_Folding (C) := (Parser.Parse_Code_Point (Field));
                     UCD.Full_Case_Folding (C) :=
                     new Wide_Wide_String'
                        (1 => Parser.Parse_Code_Point (Field));
                  when Simple => UCD.Simple_Case_Folding (C) :=
                     (Parser.Parse_Code_Point (Field));
                  when Full =>
                     UCD.Full_Case_Folding (C) :=
                     new Wide_Wide_String'
                        (Parser.Parse_Sequence (Field));
                  when Turkic => null;
               end case;
            end loop;
         when 3 =>
            if Field /= "" then
               raise Constraint_Error;
            end if;
         when others => raise Constraint_Error;
      end case;
   end Process_Case_Folding_Field;
   
   procedure Process_Case_Folding is
   new Unicode.Character_Database.Parser.Process_File (Process_Case_Folding_Field); 

   Is_NFKC_CF_Record : Boolean := False;
   
   procedure Process_Normalization_Field
   (Scope  : Code_Point_Range;
      Number : Unicode.Character_Database.Parser.Field_Number;
      Field  : Wide_Wide_String) is
   begin
      case Number is
         when 1 =>
            Is_NFKC_CF_Record := Field = "NFKC_CF";
         when 2 =>
            if not Is_NFKC_CF_Record then
               return;
            end if;
            declare 
               Value : constant Special_Mapping_Value :=
                  (if Field = "<code point>" then null
                   else new Wide_Wide_String'(Parser.Parse_Sequence (Field)));
            begin
               for C in Scope.Low .. Scope.High loop
                  UCD.Nontrivial_NFKC_Casefold (C) := Value;
               end loop;
            end;
         when others => null;
      end case;
   end Process_Normalization_Field;
   
   procedure Process_Normalization_Props is
   new Unicode.Character_Database.Parser.Process_File (Process_Normalization_Field); 

   Emoji : constant String := Ada.Directories.Compose (Directory, "emoji");
   UnicodeData : constant String := Ada.Directories.Compose (Directory, "UnicodeData", "txt");
begin

   Process_Simple_Case_Mappings (UnicodeData);
   Process_Decomposition_Mappings (UnicodeData);
   Process_Special_Casing (Ada.Directories.Compose (Directory, "SpecialCasing", "txt"));
   Process_Case_Folding (Ada.Directories.Compose (Directory, "CaseFolding", "txt"));
   Process_Normalization_Props (Ada.Directories.Compose (Directory, "DerivedNormalizationProps", "txt"));

   Process_General_Categories (UCD.General_Categories);
   Process_Bidi_Classes (UCD.Bidi_Classes);
   Process_Line_Breaking_Classes (UCD.Line_Breaking_Classes);
   Process_Scripts (UCD.Scripts);
   Process_Decomposition_Types (UCD.Decomposition_Types);
   Process_East_Asian_Widths (UCD.East_Asian_Width_Classes);

   Process_Binary_Property_File (Ada.Directories.Compose (Directory, "DerivedCoreProperties", "txt"));
   Process_Binary_Property_File (Ada.Directories.Compose (Directory, "PropList", "txt"));
   Process_Binary_Property_File (Ada.Directories.Compose (Extracted, "DerivedBinaryProperties", "txt"));
   --Process_Binary_Property_File (Ada.Directories.Compose (Emoji, "emoji-data", "txt"));
   return UCD;
end Read_UCD;

All_Versions : array (Unicode.Version) of Access_Database := (others => null);

function Version (V : Unicode.Version) return Access_Database is
   Version_Image : constant String := V'Image;
   I : constant Positive := Version_Image'First + String'("Version_")'Length;
   Directory : constant String := [for C of Version_Image (I .. Version_Image'Last) => (if C = '_' then '.' else C)];
begin
   if All_Versions (V) = null then
      All_Versions (V) := Read_UCD (Directory);
   end if;
   return All_Versions (V);
end Version;

function Latest return Access_Database is (Version (Unicode.Version'Last));

end Unicode.Character_Database;
