with Ada.Iterator_Interfaces;

with Unicode;

generic
   type Cursor is private;
   with function Has_Element (Position : Cursor) return Boolean;
   with function Scalar_Value (Position : Cursor) return Unicode.Scalar_Value;
   with package Code_Point_Iterators is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);
   type Forward_Iterator is limited new Code_Point_Iterators.Forward_Iterator with private;
package Unicode.Algorithms is

type Normalization_Quick_Check_Result is (False, True, Maybe);
type Normalization_Form is (C, D, KC, KD);

function Is_Normalized (Form   : Normalization_Form; 
                        Source : Forward_Iterator)
   return Normalization_Quick_Check_Result;

end Unicode.Algorithms;