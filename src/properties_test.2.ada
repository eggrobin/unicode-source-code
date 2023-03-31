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
         if CF /= SCF or else NFKC_CF /= NFKC_SCF then
            Ada.Wide_Wide_Text_IO.Put ("[");
         end if;
         if CF = SCF and then NFKC_CF /= NFKC_SCF then
            Ada.Wide_Wide_Text_IO.Put_Line (C & ':' & Code_Points (NFKC_CF) & ';' & Code_Points (NFKC_SCF) & ';' & Code_Points (CF) & ';' & Code_Points (SCF));
            raise Constraint_Error with Unicode.U_Notation (C);
         end if;
         if NFKC_CF = NFKC_SCF and then CF /= SCF then
            if NFKC_CF = CF then
               Ada.Wide_Wide_Text_IO.Put_Line
               ("+--] " & C & " (" & Unicode.U_Notation (C) & ") : NFKC_CF = NFKC_SCF = CF """ & NFKC_CF &
                  """ (" & Code_Points (NFKC_CF) & ") ≠ SCF """ &
                  SCF & """ (" & Code_Points (SCF) & ")");
            elsif NFKC_CF = SCF then
               Ada.Wide_Wide_Text_IO.Put_Line
               ("--+] " & C & " (" & Unicode.U_Notation (C) & ") : NFKC_CF = NFKC_SCF = SCF """ & NFKC_CF &
                  """ (" & Code_Points (NFKC_CF) & ") ≠ CF """ &
                  CF & """ (" & Code_Points (CF) & ")");
            else
               Ada.Wide_Wide_Text_IO.Put_Line
               ("-+-] " & C & " (" & Unicode.U_Notation (C) & ") : SCF """ &
                  SCF & """ (" & Code_Points (SCF) & ") ≠ NFKC_SCF = NFKC_CF """ & NFKC_CF &
                  """ (" & Code_Points (NFKC_CF) & ") ≠ CF """ &
                  CF & """ (" & Code_Points (CF) & ")");
            end if;
         end if;
         if NFKC_CF /= NFKC_SCF then
            if SCF /= NFKC_SCF then
               Ada.Wide_Wide_Text_IO.Put_Line (C & ':' & Code_Points (NFKC_CF) & ';' & Code_Points (NFKC_SCF) & ';' & Code_Points (CF) & ';' & Code_Points (SCF));
               raise Constraint_Error with Unicode.U_Notation (C);
            end if;
            if CF /= NFKC_CF then
               Ada.Wide_Wide_Text_IO.Put_Line
               ("***] " & C & " (" & Unicode.U_Notation (C) & ") : NFKC_CF """ & NFKC_CF &""" (" & Code_Points (NFKC_CF) & ") ≠ CF """ & CF &
                  """ (" & Code_Points (CF) & ") ≠ SCF=NFKC_SCF """ &
                  NFKC_SCF & """ (" & Code_Points (NFKC_SCF) & ")");
            else
               Ada.Wide_Wide_Text_IO.Put_Line
               ("---] "&C & " (" & Unicode.U_Notation (C) & ") : CF=NFKC_CF """ & NFKC_CF &
                  """ (" & Code_Points (NFKC_CF) & ") ≠ SCF=NFKC_SCF """ &
                  NFKC_SCF & """ (" & Code_Points (NFKC_SCF) & ")");
            end if;
         end if;
      end;
   end loop;
end Properties_Test;
