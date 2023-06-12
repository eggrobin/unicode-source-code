with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Integer_Wide_Wide_Text_IO;

with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Code_Point_Set;
use all type Unicode.Properties.Binary_Property;
use all type Unicode.Properties.General_Category;

procedure Properties_Test is
   type Ada_Version is (Ada_2005, Ada_2012, Ada_2022);
   Documents_In_The_Note : constant array (Ada_Version) of Unicode.Version :=
     (Ada_2005 => Unicode.Version_4_1_0,  -- TODO(egg): Should be 4.0.0.
      Ada_2012 => Unicode.Version_6_0_0,
      Ada_2022 => Unicode.Version_13_0_0);
   function Ada_Identifier_Characters
     (Version : Ada_Version) return Unicode.Code_Point_Set
   is
      UCD :
        Unicode.Character_Database.Access_Database renames
        Unicode.Character_Database.Version (Documents_In_The_Note (Version));
   begin
      return
        (UCD.Set_Of (Uppercase_Letter) or UCD.Set_Of (Lowercase_Letter) or
         UCD.Set_Of (Titlecase_Letter) or UCD.Set_Of (Modifier_Letter) or
         UCD.Set_Of (Other_Letter) or UCD.Set_Of (Letter_Number)) or
        (UCD.Set_Of (Nonspacing_Mark) or UCD.Set_Of (Spacing_Mark) or
         UCD.Set_Of (Decimal_Number) or UCD.Set_Of (Connector_Punctuation));
   end Ada_Identifier_Characters;
begin
   for Update in Ada_Version'Succ (Ada_2005) .. Ada_Version'Last loop
      declare
         Previous_Version  : Ada_Version renames Ada_Version'Pred (Update);
         Incompatibilities : constant Unicode.Code_Point_Set :=
           Ada_Identifier_Characters (Previous_Version) -
           Ada_Identifier_Characters (Update);
         Cardinality       : Natural                         := 0;
      begin
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Sequence
              (Unicode.Character_Database.Version
                 (Documents_In_The_Note (Update))
                 .Set_Of
                 (Unicode.Properties.Other_ID_Continue)));
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Sequence
              (Unicode.Character_Database.Version
                 (Documents_In_The_Note (Update))
                 .Set_Of
                 (Unicode.Properties.Other_ID_Start)));
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Sequence (Ada_Identifier_Characters (Update))'Length'
              Wide_Wide_Image &
            " characters allowed in identifiers in " & Update'Wide_Wide_Image);
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Sequence
              (Ada_Identifier_Characters (Update) -
               Ada_Identifier_Characters (Previous_Version))'
              Length'
              Wide_Wide_Image &
            " new");
         if Incompatibilities /= Unicode.Null_Set then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Characters allowed in identifiers in " &
               Previous_Version'Wide_Wide_Image & " but not in " &
               Update'Wide_Wide_Image);
            for Character_Range of To_Ranges (Incompatibilities) loop
               Ada.Wide_Wide_Text_IO.Put
                 (Unicode.U_Notation (Character_Range) & " [");
               declare
                  Length : Positive :=
                    Unicode.Code_Point'Pos (Character_Range.High) -
                    Unicode.Code_Point'Pos (Character_Range.Low) + 1;
               begin
                  Cardinality := Cardinality + Length;
                  Ada.Integer_Wide_Wide_Text_IO.Put (Length);
               end;
               Ada.Wide_Wide_Text_IO.Put_Line ("]");
            end loop;
            Ada.Wide_Wide_Text_IO.Put ("Total: ");
            Ada.Integer_Wide_Wide_Text_IO.Put (Cardinality);
         end if;
      end;
   end loop;
end Properties_Test;
