with Unicode.Encoding_Forms;
package body Unicode.Encoding_Forms is

package body UTF_16 is
   subtype Surrogate is Code_Unit range Code_Unit'Val(16#D800#) .. Code_Unit'Val(16#DFFF#);
   subtype High_Surrogate is Surrogate range Code_Unit'Val(16#D800#) .. Code_Unit'Val(16#DBFF#);
   subtype Low_Surrogate is Surrogate range Code_Unit'Val(16#DC00#) .. Code_Unit'Val(16#DFFF#);


   function Get_Supplementary (High : High_Surrogate;
                               Low  : Low_Surrogate) return Unicode.Scalar_Value is
   begin
      return Unicode.Scalar_Value'Val
         (High_Surrogate'Pos (High) * 2 ** 10 + Low_Surrogate'Pos (Low)
          - (High_Surrogate'Pos (High_Surrogate'First) / 2 ** 10 +
             Low_Surrogate'Pos (Low_Surrogate'First)));
   end Get_Supplementary;

   function Has_Element (Position : Code_Point_Cursor) return Boolean is
   begin
      return Has_Element (Position.First_Code_Unit);
   end Has_Element;

   function Make_Code_Point_Cursor (Code_Units      : Code_Unit_Iterator;
                                    First_Code_Unit : Code_Unit_Cursor) return Code_Point_Cursor is
   begin
      if not Has_Element (First_Code_Unit) then
         -- Out of range.
         return (First_Code_Unit, First_Code_Unit, Scalar_Value'Val(16#FFFF#));
      end if;
      declare 
         C : Code_Unit := Get_Code_Unit (First_Code_Unit);
      begin
         if C in Surrogate then
            return (First_Code_Unit, First_Code_Unit, Unicode.Scalar_Value'Val (Code_Unit'Pos (C)));
         else
            if C in High_Surrogate then
               declare
                  Next_Code_Unit : constant Code_Unit_Cursor := Code_Units.Next (First_Code_Unit);
               begin
                  if Has_Element (Next_Code_Unit) then
                     declare
                        C2 : Code_Unit := Get_Code_Unit (Next_Code_Unit);
                     begin
                        if C2 in Low_Surrogate then
                           return (First_Code_Unit, Next_Code_Unit, Get_Supplementary (C, C2));
                        end if;
                     end;
                  end if;
               end;
            end if;
            -- Invalid UTF-16, treat as U+FFFD.
            return (First_Code_Unit, First_Code_Unit, Scalar_Value'Val(16#FFFD#));
         end if;
      end;
   end Make_Code_Point_Cursor;

   overriding function First (Object : Code_Point_Iterator) return Code_Point_Cursor is
      (Make_Code_Point_Cursor (Object.Code_Units,
                               Object.Code_Units.First));
   overriding function Next (Object   : Code_Point_Iterator;
                             Position : Code_Point_Cursor) return Code_Point_Cursor is
      (Make_Code_Point_Cursor (Object.Code_Units,
                               Object.Code_Units.Next (Position.Last_Code_Unit)));
   function Get_Code_Point (Position : Code_Point_Cursor) return Unicode.Scalar_Value is
      (Position.Code_Point);
end UTF_16;

end Unicode.Encoding_Forms;
