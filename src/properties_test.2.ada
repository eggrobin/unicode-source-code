with Ada.Exceptions;
with Ada.Text_IO;
with Unicode;

procedure Properties_Test is

   subtype High_Surrogate is Wide_Character
      range Wide_Character'Val (16#D800#) .. Wide_Character'Val (16#DB7F#);
   subtype Low_Surrogate is Wide_Character
      range Wide_Character'Val (16#DC00#) .. Wide_Character'Val (16#DFFF#);
   subtype UTF_16_String is Wide_String
      with Dynamic_Predicate =>
      (for all I in UTF_16_String'Range =>
         (if UTF_16_String (I) in High_Surrogate then
            I + 1 in UTF_16_String'Range and then
               UTF_16_String (I + 1) in Low_Surrogate));
   subtype Code_Point is Wide_Wide_Character range
      Wide_Wide_Character'Val (0) .. Wide_Wide_Character'Val (16#10FFFF#);
   subtype Scalar_Value is Code_Point with
      Static_Predicate => Scalar_Value not in
         Code_Point'Val (16#D800#) .. Code_Point'Val (16#DFFF#);
   subtype UTF_32_String is Wide_Wide_String
      with Dynamic_Predicate => (for all Code_Unit of UTF_32_String => Code_Unit in Scalar_Value);

   E_Acute  : String      := Character'Val(16#C3#) & Character'Val(16#A9#);
   Egg      : UTF_16_String := Wide_Character'Val(16#D808#) & Wide_Character'Val(16#DE6D#);
   Eggs     : UTF_16_String := Egg & Egg;
   Wide_Wide_Egg : UTF_32_String := "𒉭𒉭";

   function Code_Point_Count (S : Unicode.Code_Point_Sequence'Class) return Natural is
      Count : Natural := 0;
   begin
      for CP of S loop
         Count := Count + 1;
      end loop;
      return Count;
   end Code_Point_Count;

begin
   Ada.Text_IO.Put_Line (Boolean'Image (Egg in UTF_16_String));
   Ada.Text_IO.Put_Line (Boolean'Image (Eggs in UTF_16_String));
   Ada.Text_IO.Put_Line (Boolean'Image (Wide_Wide_Egg in UTF_32_String));
   Egg := Eggs (1 .. 2);
   Ada.Text_IO.Put_Line (Boolean'Image (Egg in UTF_16_String));
   Egg := Eggs (2 .. 3);
   Ada.Text_IO.Put_Line (Boolean'Image (Egg in UTF_16_String));
   Wide_Wide_Egg (1) := Wide_Wide_Character'Val (Wide_Character'Pos (Egg (1)));
   Ada.Text_IO.Put_Line (Boolean'Image (Wide_Wide_Egg in UTF_32_String));
end Properties_Test;
