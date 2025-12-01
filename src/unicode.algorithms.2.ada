with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Properties.Quick_Check_Result;
use all type Unicode.Properties.Canonical_Combining_Class;

package body Unicode.Algorithms is

function Is_Normalized (Form   : Normalization_Form; 
                        Source : Forward_Iterator)
return Unicode.Properties.Quick_Check_Result is
   Last_Canonical_Class : Unicode.Properties.Canonical_Combining_Class
                        := Not_Reordered;
   Result               : Unicode.Properties.Quick_Check_Result := Yes;
   C                    : Cursor := Source.First;
   UCD                  : constant Unicode.Character_Database.Access_Database
                        := Unicode.Character_Database.Latest;
begin
   while Has_Element (C) loop
      declare 
         CP : constant Unicode.Scalar_Value := Get_Code_Point (C);
         Canonical_Class : Unicode.Properties.Canonical_Combining_Class
                           := UCD.Get_Canonical_Combining_Class (CP);
      begin
         if Last_Canonical_Class > Canonical_Class and then
            Canonical_Class /= Not_Reordered then
               return No;
         end if;
         Last_Canonical_Class := Canonical_Class;
         case UCD.Normalization_Quick_Check (Form, CP) is
            when No    => return No;
            when Maybe => Result := Maybe;
            when Yes   => null;
         end case;
      end;
      C := Source.Next (C);
   end loop;
   return Result;
end Is_Normalized;

end Unicode.Algorithms;