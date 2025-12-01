with Ada.Strings.Wide_Wide_Maps;

package Unicode is

   -- D10.
   subtype Code_Point is Wide_Wide_Character range
     Wide_Wide_Character'Val (0) .. Wide_Wide_Character'Val (16#10FFFF#);
   -- D76.
   subtype Scalar_Value is Code_Point with
      Static_Predicate => Scalar_Value not in
         Code_Point'Val (16#D800#) .. Code_Point'Val (16#DFFF#);

   subtype Code_Point_Set is Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
   use all type Code_Point_Set;
   Null_Set : Code_Point_Set renames Ada.Strings.Wide_Wide_Maps.Null_Set;
   Codespace : constant Code_Point_Set := To_Set (Span => (Code_Point'First,
                                                           Code_Point'Last));

   subtype Code_Point_Range is
     Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
   use all type Code_Point_Range;

   type Range_Style is (Two_Dots, En_Dash);

   -- Returns a string representing the code point in the notation U+n
   -- described under Code Points in Section A.1 of The Unicode Standard.
   -- https://www.unicode.org/versions/Unicode15.0.0/appA.pdf#G7083.
   function U_Notation (C : Code_Point) return String;
   -- Returns a string representing the range as U+n..U+m, or U+n for a
   -- singleton range.
   function U_Notation (R : Code_Point_Range) return String;

   function U_Notation (C : Code_Point) return Wide_String;
   function U_Notation (R     : Code_Point_Range;
                        Style : Range_Style := En_Dash) return Wide_String;

   function U_Notation (C : Code_Point) return Wide_Wide_String;
   function U_Notation (R     : Code_Point_Range;
                        Style : Range_Style := En_Dash) return Wide_Wide_String;

   type Version is (Version_4_1_0,
                    Version_5_0_0,
                    Version_5_1_0,
                    Version_5_2_0,
                    Version_6_0_0,
                    Version_6_1_0,
                    Version_6_2_0,
                    Version_6_3_0,
                    Version_7_0_0,
                    Version_8_0_0,
                    Version_9_0_0,
                    Version_10_0_0,
                    Version_11_0_0,
                    Version_12_0_0,
                    Version_12_1_0,
                    Version_13_0_0,
                    Version_14_0_0,
                    Version_15_0_0);

   type Normalization_Form is (D, C, KD, KC);
end Unicode;
