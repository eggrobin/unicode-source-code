with Ada.Integer_Text_IO;

package body Unicode is
         
   subtype Code_Point_Pos is Natural range
     Code_Point'Pos (Code_Point'First) .. Code_Point'Pos (Code_Point'Last);

   generic 
      type Character is (<>);
      type String is array (Positive range <>) of Character;
   package Character_Dependent_Implementations is
      function U_Notation (C : Code_Point) return String;
      -- Uses two dots; for use with character types that do not have en dash.
      function U_Notation (R : Code_Point_Range) return String;
      function U_Notation (R     : Code_Point_Range;
                           Style : Range_Style) return String;
   end Character_Dependent_Implementations;
   
   package body Character_Dependent_Implementations is
      subtype Basic_Latin is Standard.Character range
        Standard.Character'Val (0) .. Standard.Character'Val (16#7F#);
      type Basic_Latin_String is array (Positive range <>) of Basic_Latin;
      
      function To_Character (C : Basic_Latin) return Character is
        (Character'Val (Basic_Latin'Pos (C)));
      
      function To_String (S : Basic_Latin_String) return String is
         Result : String (S'First .. S'Last);
      begin
         for I in Result'Range loop
            Result (I) := To_Character (S (I));
         end loop;
         return Result;
      end To_String;
      
      type Hex_Digit is mod 16;
      Hex_Digits : array (Hex_Digit) of Character;

      function U_Notation (C : Code_Point) return String is
         
         Pos    : constant Code_Point_Pos := Code_Point'Pos (C);
         Result : String (1 ..
                            2 + (case Pos is 
                                 when 0 .. 16#FFFF#             => 4,
                                 when 16#10000# .. 16#FFFFF#    => 5,
                                 when 16#100000# .. 16#10FFFF#  => 6)) :=
              (1 => To_Character ('U'), 2 => To_Character ('+'), others => <>);

      begin
         for I in Positive range 3 .. Result'Last loop
            Result (I) :=
              Hex_Digits (Hex_Digit'Mod (Pos / 16 ** (Result'Last - I)));
         end loop;
         return Result;
      end U_Notation;
      
      function U_Notation (R : Code_Point_Range) return String is
        (U_Notation (R, Two_Dots));
   
      function U_Notation (R : Code_Point_Range; Style : Range_Style) 
                           return String is
        (if R.Low = R.High then U_Notation (R.Low)
         else U_Notation (R.Low) &
         (case Style is 
               when Two_Dots => To_String (".."), 
               when En_Dash  => (if Character'Pos (Character'Last) >= 16#2013#
                                 then (1 => Character'Val (16#2013#))
                                 else raise Constraint_Error with
                                   "No en dash in character type")) &
           U_Notation (R.High));
   begin
      for D in Hex_Digit range 0 .. 9 loop
         Hex_Digits (D) := Character'Val (Basic_Latin'Pos ('0') + Natural (D));
      end loop;
      for D in Hex_Digit range 16#A# .. 16#F# loop
         Hex_Digits (D) := Character'Val (Basic_Latin'Pos ('A') +
                                            Natural (D - 16#A#));
      end loop;
   end Character_Dependent_Implementations;
   
   package Implementations is
     new Character_Dependent_Implementations (Character, String);
   package Wide_Implementations is
     new Character_Dependent_Implementations (Wide_Character, Wide_String);
   package Wide_Wide_Implementations is 
     new Character_Dependent_Implementations (Wide_Wide_Character,
                                              Wide_Wide_String);
   
   function U_Notation (C : Code_Point) return String renames
     Implementations.U_Notation;   
   function U_Notation (R : Code_Point_Range) return String renames
     Implementations.U_Notation;
   
   function U_Notation (C : Code_Point) return Wide_String renames
     Wide_Implementations.U_Notation;
   function U_Notation (R     : Code_Point_Range;
                        Style : Range_Style := En_Dash) return Wide_String
                        renames Wide_Implementations.U_Notation;
   
   function U_Notation (C : Code_Point) return Wide_Wide_String renames
     Wide_Wide_Implementations.U_Notation;
   function U_Notation (R     : Code_Point_Range;
                        Style : Range_Style := En_Dash) return Wide_Wide_String
                        renames Wide_Wide_Implementations.U_Notation;
   
end Unicode;
