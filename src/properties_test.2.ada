with Ada.Wide_Wide_Text_IO;

with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Properties.Binary_Property;

procedure Properties_Test is
   UCD : Unicode.Character_Database.Database renames Unicode.Character_Database.Latest.all;

   procedure Check
     (Changes      : Unicode.Properties.Binary_Property;
      Mapping      : access function
        (UCD : Unicode.Character_Database.Database; C : Unicode.Code_Point) return Wide_Wide_String;
      Mapping_Name : Wide_Wide_String) is
      Passed : constant Boolean :=
        (for all C in Unicode.Code_Point =>
           UCD.Get (Changes, C) = (Mapping (UCD, C) /= (1 => C)));
      Test   : constant Wide_Wide_String :=
        "(∀ C ∈ Code_Points, " & Changes'Wide_Wide_Image & " (C) = (" &
        Mapping_Name & " (C) ≠ C))";
   begin
         Ada.Wide_Wide_Text_IO.Put_Line (Test & " = " & Passed'Wide_Wide_Image);
      if not Passed then
         for C in Unicode.Code_Point loop
            if UCD.Get (Changes, C) /=
              (Mapping (UCD, C) /= (1 => C)) then
               Ada.Wide_Wide_Text_IO.Put
                 (Unicode.U_Notation (C) & ' ' & C & ": " &
                    Changes'Wide_Wide_Image &
                    "=" & UCD.Get (Changes, C)'Wide_Wide_Image &
                    " but its " & Mapping_Name & " is");
               for M of Mapping (UCD, C) loop
                  Ada.Wide_Wide_Text_IO.Put (' ' & Unicode.U_Notation (M));
               end loop;
               Ada.Wide_Wide_Text_IO.Put_Line (' ' & Mapping (UCD, C));
            end if;
         end loop;
      end if;
   end Check;
begin
   Check (Changes_When_Lowercased, Unicode.Character_Database.Lowercase_Mapping'Access,
          "Lowercase_Mapping");
   Check (Changes_When_Uppercased, Unicode.Character_Database.Uppercase_Mapping'Access,
          "Uppercase_Mapping");
   Check (Changes_When_Titlecased, Unicode.Character_Database.Titlecase_Mapping'Access,
          "Titlecase_Mapping");
   Check (Changes_When_Casefolded, Unicode.Character_Database.Case_Folding'Access,
          "Case_Folding");
end Properties_Test;
