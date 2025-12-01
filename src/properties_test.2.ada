with Ada.Exceptions;
with Ada.Text_IO;

with Unicode.Algorithms;
with Unicode.Properties;
With Unicode.Encoding_Forms;

procedure Properties_Test is
   Test_String : Wide_String :=
      (Wide_Character'Val(16#D808#), Wide_Character'Val(16#DE6D#),
       'e',
       Wide_Character'Val(16#0301#));
begin
   for I in Test_String'Range loop
      for J in I .. Test_String'Last loop
         declare
            Test_Substring : Wide_String renames Test_String (I .. J);
            package UTF_16_Substring is new Unicode.Encoding_Forms.UTF_16_Wide_Strings (Test_Substring);
            use UTF_16_Substring.UTF_16_S_Iterators;
            package Substring_Algorithms is new Unicode.Algorithms
               (Code_Point_Cursor, Has_Element, Get_Code_Point, Code_Point_Iterators, Code_Point_Iterator);
            Code_Points : Code_Point_Iterator;
         begin
            for Form in Unicode.Normalization_Form loop
               Ada.Text_IO.Put_Line (
                  "NF" & Unicode.Normalization_Form'Image (Form) & ": " &
                  Unicode.Properties.Quick_Check_Result'Image (
                     Substring_Algorithms.Is_Normalized (Form, Code_Points)));
            end loop;
         end;
      end loop;
   end loop;   
end Properties_Test;
