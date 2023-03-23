with Ada.Wide_Wide_Text_IO;

with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Properties.Binary_Property;
use all type Unicode.Properties.General_Category;

procedure Properties_Test is
   package UCD renames Unicode.Character_Database;
   function Code_Points (S : Wide_Wide_String) return Wide_Wide_String is
     (case S'Length is when 0 => "",
        when 1 => Unicode.U_Notation (S (S'First)),
        when others =>
          Unicode.U_Notation (S (S'First)) & ", " &
          Code_Points (S (S'First + 1 .. S'Last)));
begin
   for C in Unicode.Code_Point loop
      declare
         CF  : Wide_Wide_String renames UCD.Latest.Case_Folding (C);
         SCF : Wide_Wide_String := [UCD.Latest.Simple_Case_Folding (C)];
         NFKC_CF  : Wide_Wide_String renames UCD.Latest.NFKC_Casefold (C);
         NFKC_SCF : Wide_Wide_String renames UCD.Latest.NFKC_SimpleCasefold (C);
      begin
      if (NFKC_CF = NFKC_SCF) /= (CF = SCF) then
         raise Constraint_Error with C;
      end if;
      if NFKC_CF /= NFKC_SCF then
      Ada.Wide_Wide_Text_IO.Put_Line
        (C & " (" & Unicode.U_Notation (C) & ") : NFKC_CF """ & NFKC_CF &
         """ (" & Code_Points (NFKC_CF) & ") ≠ NFKC_SCF """ &
         NFKC_SCF & """ (" & Code_Points (NFKC_SCF) & ")");
      end if;
      end;
   end loop;
end Properties_Test;
