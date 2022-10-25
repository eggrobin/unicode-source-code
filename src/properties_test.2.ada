with Ada.Wide_Wide_Text_IO;

with Unicode.Properties;

use all type Unicode.Properties.Binary_Property;

procedure Properties_Test is
   procedure Check
     (Changes      : Unicode.Properties.Binary_Property;
      Mapping      : access function
        (C : Unicode.Code_Point) return Wide_Wide_String;
      Mapping_Name : Wide_Wide_String) is
      Passed : constant Boolean :=
        (for all C in Unicode.Code_Point =>
           Unicode.Properties.Get (Changes, C) = (Mapping (C) /= (1 => C)));
      Test   : constant Wide_Wide_String :=
        "(∀ C ∈ Code_Points, " & Changes'Wide_Wide_Image & " (C) = (" &
        Mapping_Name & " (C) ≠ C))";
   begin
         Ada.Wide_Wide_Text_IO.Put_Line (Test & " = " & Passed'Wide_Wide_Image);
      if not Passed then
         for C in Unicode.Code_Point loop
            if Unicode.Properties.Get (Changes, C) /=
              (Mapping (C) /= (1 => C)) then
               Ada.Wide_Wide_Text_IO.Put
                 (Unicode.U_Notation (C) & ' ' & C & ": " &
                    Changes'Wide_Wide_Image &
                    "=" & Unicode.Properties.Get (Changes, C)'Wide_Wide_Image &
                    " but its " & Mapping_Name & " is ");
               for M of Mapping (C) loop
                  Ada.Wide_Wide_Text_IO.Put (' ' & Unicode.U_Notation (M));
               end loop;
               Ada.Wide_Wide_Text_IO.Put_Line (' ' & Mapping (C));
            end if;
         end loop;
      end if;
   end Check;
begin
   Check (Changes_When_Lowercased, Unicode.Properties.Lowercase_Mapping'Access,
          "Lowercase_Mapping");
   Check (Changes_When_Uppercased, Unicode.Properties.Uppercase_Mapping'Access,
          "Uppercase_Mapping");
   Check (Changes_When_Titlecased, Unicode.Properties.Titlecase_Mapping'Access,
          "Titlecase_Mapping");
   Check (Changes_When_Casefolded, Unicode.Properties.Case_Folding'Access,
          "Case_Folding");
end Properties_Test;
