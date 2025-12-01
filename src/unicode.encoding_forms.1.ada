With Ada.Iterator_Interfaces;

package Unicode.Encoding_Forms is

generic
   type Code_Unit is (<>);
   type Code_Unit_Cursor is private;
   with function Has_Element (Position : Code_Unit_Cursor) return Boolean;
   with function Get_Code_Unit (Position : Code_Unit_Cursor) return Code_Unit;
   with package Code_Unit_Iterators is
      new Ada.Iterator_Interfaces (Code_Unit_Cursor, Has_Element);
   type Code_Unit_Iterator is limited new Code_Unit_Iterators.Forward_Iterator with private;
package UTF_16 is
   type Code_Point_Cursor is private;
   function Has_Element (Position : Code_Point_Cursor) return Boolean;
   package Code_Point_Iterators is
      new Ada.Iterator_Interfaces (Code_Point_Cursor, Has_Element);
   type Code_Point_Iterator is limited new Code_Point_Iterators.Forward_Iterator with
      record
         Code_Units : Code_Unit_Iterator;
      end record;

   overriding function First (Object : Code_Point_Iterator) return Code_Point_Cursor;
   overriding function Next (Object   : Code_Point_Iterator;
                             Position : Code_Point_Cursor) return Code_Point_Cursor;
   function Get_Code_Point (Position : Code_Point_Cursor) return Unicode.Scalar_Value;
private
   type Code_Point_Cursor is
      record
         First_Code_Unit : Code_Unit_Cursor;
         Last_Code_Unit  : Code_Unit_Cursor;
         Code_Point      : Unicode.Scalar_Value;
      end record;
end UTF_16;

generic
   S : Wide_String;
package UTF_16_Wide_Strings is
   subtype Index is Positive range S'Range;
   function Has_Element (Position : Index) return Boolean is (Position in Index);
   function Get_Code_Unit (Position : Index) return Wide_Character is (S (Position));
   package S_Iterators is new Ada.Iterator_Interfaces (Index, Has_Element);
   type S_Iterator is new S_Iterators.Forward_Iterator with null record;
   overriding function First (Object : S_Iterator) return Index is (S'First);
   overriding function Next (Object   : S_Iterator;
                             Position : Index) return Index is (Position + 1);
   package UTF_16_S_Iterators is new UTF_16 (Wide_Character, Index, Has_Element, Get_Code_Unit, S_Iterators, S_Iterator);
end UTF_16_Wide_Strings;


end Unicode.Encoding_Forms;