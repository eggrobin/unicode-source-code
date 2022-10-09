with Ada.Sequential_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Unicode.IO is

   function Read_File (Name : String)
                       return Ada.Strings.UTF_Encoding.UTF_String is
      type Byte is mod 16#100#;
      for Byte'Size use 8;
      B : Byte;
      package Byte_IO is new Ada.Sequential_IO (Byte);
      use Byte_IO;
      File : Byte_IO.File_Type;
   begin
      Byte_IO.Open (File, In_File, Name);
      for Size in Natural loop
         if Byte_IO.End_Of_File (File) then
            declare
               Encoded_Text : Ada.Strings.UTF_Encoding.UTF_String (1 .. Size);
            begin
               Byte_IO.Reset (File);
               for I in Encoded_Text'Range loop
                  Byte_IO.Read (File, B);
                  Encoded_Text (I) := Character'Val (B); 
               end loop;
               return Encoded_Text;
            end;
         end if;
         Byte_IO.Read (File, B);
      end loop;
      raise Constraint_Error with "File " & Name & " is too big";
   end Read_File;
   
   function Read_File
     (Name             : String;
      Default_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
      Actual_Encoding  : out Ada.Strings.UTF_Encoding.Encoding_Scheme;
      Uses_BOM         : out Boolean) return Wide_Wide_String is
      use Ada.Strings.UTF_Encoding;
      function BOM (Scheme : Encoding_Scheme) return UTF_String is
        (case Scheme is 
            when UTF_8    => BOM_8,
            when UTF_16LE => BOM_16LE,
            when UTF_16BE => BOM_16BE);
              
      Encoded_Text : Ada.Strings.UTF_Encoding.UTF_String := Read_File (Name);
   begin
      Actual_Encoding := Encoding (Encoded_Text, Default_Encoding);
      Uses_Bom := Actual_Encoding /= Default_Encoding or else
        (Encoded_Text'Length >= BOM (Actual_Encoding)'Length and then
         Encoded_Text (Encoded_Text'First ..
                           Encoded_Text'First +
                             BOM (Actual_Encoding)'Length - 1) =
           Bom (Actual_Encoding));
      return Wide_Wide_Strings.Decode (Encoded_Text, Actual_Encoding);
   end Read_File;
   
   function Read_File (Name     : String;
                       Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme)
                       return Wide_Wide_String is
     (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
        (Read_File (Name), Encoding));

end Unicode.IO;
