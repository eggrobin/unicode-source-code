with Ada.Strings.Fixed;
with Ada.Wide_Wide_Text_IO;

use Ada.Strings.Fixed;

procedure Test is
   procedure רשם‎ (Message : String) is null;
   הודעה‎ : String := "42";

   Spoof : constant String := 7 * "1" & "0";

   type א‎ is new Integer;
   I : aliased א‎ := 2;
   A : not null access א‎ := I'Access;
begin
   --⁨‫متغیر A خالی نیست.
   A.all := A.all + 1;
   A := new א‎'(2);
   --⁨متغیر A خالی نیست.
   I := A.all;
   <<שגיאה‎>> רשם‎ (הודעה‎); --⁨ משהו השתבש.
   for Hebrew_Letter in Wide_Character range 'ת'‎ .. 'א'‎ loop
      null;
   end loop;

   -- This string literal is wrapped in RLE/PDF.
   Ada.Wide_Wide_Text_IO.Put_Line ("‫YouTube تابعة لشركة Google‬"‎);
   -- The one below has an RLE, but no PDF.
   -- A diagnostic shall be emitted about it.
   Ada.Wide_Wide_Text_IO.Put_Line ("‫YouTube تابعة لشركة Google");
   -- The one below has an unmatched RLE, but it lies between an FSI and the
   -- matching PDI.
   -- This is fine.
   Ada.Wide_Wide_Text_IO.Put_Line ("⁨‫YouTube تابعة لشركة Google⁩"‎);
end Test;
