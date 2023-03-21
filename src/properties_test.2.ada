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
   for V in Unicode.Version range Unicode.Version_5_2_0 .. Unicode.Version'Last
   loop
      if UCD.Version (V).Get_General_Category ('a') = Unassigned then
         raise Constraint_Error;
      end if;
   end loop;
   for U in Unicode.Version range Unicode.Version_5_2_0 .. Unicode.Version'Last
   loop
      for C in Unicode.Code_Point loop
         if UCD.Version (U).Get_General_Category (C) /= Unassigned then
            declare
               NFKC_Casefold_at_U :
                 Wide_Wide_String renames UCD.Version (U).NFKC_Casefold (C);
            begin
               for V in Unicode.Version range U .. Unicode.Version'Last loop
                  declare
                     NFKC_Casefold_at_V :
                       Wide_Wide_String renames
                       UCD.Version (V).NFKC_Casefold (C);
                  begin
                     if NFKC_Casefold_at_V /= NFKC_Casefold_at_U then
                        if UCD.Version (U).Get (XID_Continue, C) then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("!!! NFKC_Casefold (" & Unicode.U_Notation (C) &
                              ") is " & Code_Points (NFKC_Casefold_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Code_Points (NFKC_Casefold_at_V) &" in "&
                              V'Wide_Wide_Image);
                        else
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("--- NFKC_Casefold (" & Unicode.U_Notation (C) &
                              ") is " & Code_Points (NFKC_Casefold_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Code_Points (NFKC_Casefold_at_V) &" in "&
                              V'Wide_Wide_Image);
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end loop;
end Properties_Test;
