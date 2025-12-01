with Ada.Iterator_Interfaces;

with Unicode;
with Unicode.Properties;

generic
   type Cursor is private;
   with function Has_Element (Position : Cursor) return Boolean;
   with function Get_Code_Point (Position : Cursor) return Unicode.Scalar_Value;
   with package Code_Point_Iterators is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);
   type Forward_Iterator is limited new Code_Point_Iterators.Forward_Iterator with private;
package Unicode.Algorithms is

function Is_Normalized (Form   : Normalization_Form; 
                        Source : Forward_Iterator)
   return Unicode.Properties.Quick_Check_Result;

end Unicode.Algorithms;