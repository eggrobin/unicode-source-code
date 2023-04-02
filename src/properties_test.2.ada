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
   for V in Unicode.Version range Unicode.Version_4_1_0 .. Unicode.Version'Last
   loop
      if UCD.Version (V).Get_General_Category ('a') = Unassigned then
         raise Constraint_Error;
      end if;
   end loop;
   for U in Unicode.Version range Unicode.Version_4_1_0 .. Unicode.Version'Last
   loop
      for C in Unicode.Code_Point loop
         if UCD.Version (U).Get_General_Category (C) /= Unassigned then
            declare
               SCF_at_U :
                 Unicode.Code_Point renames UCD.Version (U).Simple_Case_Folding (C);
               CF_at_U :
                 Wide_Wide_String renames UCD.Version (U).Case_Folding (C);
            begin
               for V in Unicode.Version range U .. Unicode.Version'Last loop
                  declare
                     SCF_at_V :
                       Unicode.Code_Point renames
                       UCD.Version (V).Simple_Case_Folding (C);
                     CF_at_V :
                       Wide_Wide_String renames UCD.Version (V).Case_Folding (C);
                  begin
                     if SCF_at_V /= SCF_at_U then
                        if UCD.Version (U).Get (XID_Continue, C) then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("!!! SCF (" & Unicode.U_Notation (C) &
                              ") is " & Unicode.U_Notation (SCF_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Unicode.U_Notation (SCF_at_V) &" in "&
                              V'Wide_Wide_Image);
                        else
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("--- SCF (" & Unicode.U_Notation (C) &
                              ") is " & Unicode.U_Notation (SCF_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Unicode.U_Notation (SCF_at_V) &" in "&
                              V'Wide_Wide_Image);
                        end if;
                     end if;
                     if CF_at_V /= CF_at_U then
                        if UCD.Version (U).Get (XID_Continue, C) then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("!!! CF (" & Unicode.U_Notation (C) &
                              ") is " & Code_Points (CF_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Code_Points (CF_at_V) &" in "&
                              V'Wide_Wide_Image);
                        else
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("--- CF (" & Unicode.U_Notation (C) &
                              ") is " & Code_Points (CF_at_U) &
                              " in " & U'Wide_Wide_Image & " and " &
                              Code_Points (CF_at_V) &" in "&
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
