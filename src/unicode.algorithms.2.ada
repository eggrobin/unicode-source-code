with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Properties.Canonical_Combining_Class;

package body Unicode.Algorithms is

function Is_Normalized (Form   : Normalization_Form; 
                        Source : Forward_Iterator)
return Normalization_Quick_Check_Result is
   Last_Canonical_Class : Unicode.Properties.Canonical_Combining_Class
                        := Not_Reordered;
   Result               : Normalization_Quick_Check_Result := True;
   C                    : Cursor := Source.First;
   UCD                  : constant Unicode.Character_Database.Access_Database
                        := Unicode.Character_Database.Latest;
begin
   while Has_Element (C) loop
      declare 
        Canonical_Class : Unicode.Properties.Canonical_Combining_Class
                        := UCD.Get_Canonical_Combining_Class
                              (Scalar_Value (C));
      begin
         if Last_Canonical_Class > Canonical_Class and then
            Canonical_Class /= Not_Reordered then
               return False;
         end if;
         Last_Canonical_Class := Canonical_Class;
      end;
      Result := Maybe;
      C := Source.Next (C);
   end loop;
   return Result;
end Is_Normalized;

end Unicode.Algorithms;