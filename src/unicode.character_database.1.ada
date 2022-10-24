package Unicode.Character_Database is

   -- Comments prefixed by a section number are citations of UAX #44,
   -- Unicode Version 15.0.0.
   
   -- Parsing of some of the field types described in 4.2.
   
   function Parse_Code_Point (Hex : Wide_Wide_String) return Code_Point;
  
   function Parse_Sequence (Field : Wide_Wide_String) return Wide_Wide_String;
   
   function Parse_Range (Field : Wide_Wide_String) return Code_Point_Range;
   
   -- 4.2.1: The fields are numbered starting with zero.
   type Field_Number is new Natural;
   
   generic
      with procedure Process_Field
        (Scope  : Code_Point_Range;
         Number : Field_Number;
         Field  : Wide_Wide_String);
   procedure Process_File (File_Name : String);                        

end Unicode.Character_Database;
