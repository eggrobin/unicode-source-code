with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Unicode;
with Unicode.Properties; use Unicode.Properties;

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
   for Line_Breaking_Class in Line_Break loop
      Print_Class (Line_Break_Aliases (Line_Breaking_Class).all,
                   Set_Of (Line_Breaking_Class));
   end loop;
   Print_Class ("[SA&[p{Mn}p{Mc}]]",
                Set_Of (Line_Break'(Complex_Context)) and
                  (Set_Of (Mn) or Set_Of (Mc)));
   Print_Class ("[SA-[p{Mn}p{Mc}]]",
                Set_Of (Line_Break'(Complex_Context)) -
                  (Set_Of (Mn) or Set_Of (Mc)));
   Print_Class ("[^BK,CR,LF,NL,SP,ZW]",
                Unicode.Codespace - (Set_Of (Line_Break'(Mandatory_Break))
                  or Set_Of (Line_Break'(Carriage_Return))
                  or Set_Of (Line_Break'(Line_Feed))
                  or Set_Of (Line_Break'(Next_Line))
                  or Set_Of (Line_Break'(Space))
                  or Set_Of (Line_Break'(ZWSpace))));
   Print_Class ("[^SP,BA,HY]",
                Unicode.Codespace - (Set_Of (Line_Break'(Space))
                  or Set_Of (Line_Break'(Break_After))
                  or Set_Of (Line_Break'(Hyphen))));
   Print_Class ("[OP-[p{ea=F}p{ea=W}p{ea=H}]]",
                (Set_Of (Line_Break'(Open_Punctuation)) -
                 (Set_Of (East_Asian_Width'(Fullwidth))
                    or Set_Of (East_Asian_Width'(Wide))
                    or Set_Of (East_Asian_Width'(Halfwidth)))));
   Print_Class ("[CP-[p{ea=F}p{ea=W}p{ea=H}]]",
                (Set_Of (Line_Break'(Close_Parenthesis)) -
                 (Set_Of (East_Asian_Width'(Fullwidth))
                    or Set_Of (East_Asian_Width'(Wide))
                    or Set_Of (East_Asian_Width'(Halfwidth)))));
   Print_Class ("[^RI]",
                Unicode.Codespace - (Set_Of (Line_Break'(Regional_Indicator))));
   Print_Class ("[p{Extended_Pictographic}&p{Cn}]",
                Set_Of (Extended_Pictographic) and Set_Of(Cn));
                
end Compute_Line_Breaking_Algorithm_Classes;
