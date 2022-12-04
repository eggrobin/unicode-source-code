with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Unicode;
with Unicode.Properties;

use all type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
use all type Unicode.Code_Point_Set;

procedure Compute_Line_Breaking_Algorithm_Classes is
   function To_Escaped_JS_Regex (Set : Unicode.Code_Point_Set)
                                 return Wide_Wide_String is
      Result : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   begin
      if Set = Unicode.Null_Set then
         return "";
      end if;
      Result := Result & "[";
      for R of To_Ranges (Set) loop
         declare
            Low  : Wide_Wide_String renames Unicode.U_Notation (R.Low);
            High : Wide_Wide_String renames Unicode.U_Notation (R.High);
         begin
            Result := Result & "\\u{" & Low (Low'First + 2 .. Low'Last) &
              "}-\\u{" & High (High'First + 2 .. High'Last) & "}";
         end;
      end loop;
      Result := Result & "]";
      return To_Wide_Wide_String (Result);
   end To_Escaped_JS_Regex;
   procedure Print_Class (Name : Wide_Wide_String;
                          Set  : Unicode.Code_Point_Set) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("[""" & Name & """,""" &
           To_Escaped_JS_Regex (Set) & """],");
   end Print_Class;
begin
   for Class in Unicode.Properties.Line_Break loop
      Print_Class (Unicode.Properties.Line_Break_Aliases (Class).all,
                   Unicode.Properties.Set_Of (Class));
   end loop;
end Compute_Line_Breaking_Algorithm_Classes;
