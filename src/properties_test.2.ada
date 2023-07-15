with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Containers.Generic_Array_Sort;

with Unicode;
with Unicode.Character_Database;
with Unicode.Properties;

use all type Unicode.Properties.General_Category;
use type Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;

procedure Properties_Test is
   function Size (Set : Unicode.Code_Point_Set) return Natural is
      N : Natural := 0;
   begin
      for R of Ada.Strings.Wide_Wide_Maps.To_Ranges (Set) loop
         N := N + (Unicode.Code_Point'Pos(R.High) - Unicode.Code_Point'Pos(R.Low)) + 1;
      end loop;
      return N;
   end Size;
   type Line is
      record
         Script      : Unicode.Properties.Script;
         Code_Points : Natural;
      end record;
   package UCD renames Unicode.Character_Database;
   N : constant Positive := Unicode.Properties.Script'Pos(Unicode.Properties.Script'Last);
   type A is array (Positive range <>) of Line;
   Code_Points_By_Script : A (1 .. N)
                         := [for I in 1 .. N =>
                             (Unicode.Properties.Script'Val(I),
                             Size (UCD.Latest.Set_Of(Unicode.Properties.Script'Val(I)) and
                                   (UCD.Latest.Set_Of(Uppercase_Letter) or
                                    UCD.Latest.Set_Of(Lowercase_Letter) or
                                    UCD.Latest.Set_Of(Titlecase_Letter) or
                                    UCD.Latest.Set_Of(Modifier_Letter) or
                                    UCD.Latest.Set_Of(Other_Letter))))];
   function "<" (Left, Right : Line)
      return Boolean is (Left.Code_Points < Right.Code_Points);
   procedure Sort is new Ada.Containers.Generic_Array_Sort
      (Positive, Line, A);
begin
   Sort (Code_Points_By_Script);
   for L of Code_Points_By_Script loop
      Ada.Text_IO.Put (Positive'Image (L.Code_Points) & ' ' & Unicode.Properties.Script'Image (L.Script));
      Ada.Text_IO.New_Line;
   end loop;
end Properties_Test;
