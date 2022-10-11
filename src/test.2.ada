procedure Test is
   procedure רשם (Message : String) is null;
   הודעה : String := "42";
begin
   <<שגיאה>> רשם (הודעה); -- משהו השתבש.
   for Hebrew_Letter in Wide_Character range 'ת' .. 'א' loop
      null;
   end loop;
end Test;
