package Unicode.Character_Database is

   -- Comments prefixed by a section number are citations of UAX #44,
   -- Unicode Version 15.0.0.
   
   -- 4.2.1: The fields are numbered starting with zero.
   type Field_Number is new Natural;
   
   generic
      with procedure Process_Field
        (Scope  : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Range;
         Number : Field_Number;
         Field  : Wide_Wide_String);
   procedure Process_File (File_Name : String);                        

end Unicode.Character_Database;
