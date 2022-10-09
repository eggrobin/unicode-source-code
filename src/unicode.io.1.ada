with Ada.Strings.UTF_Encoding;

package Unicode.IO is
   
   -- Reads an entire text file to a Wide_Wide_String.  This avoids depending on
   -- the implementation-defined control of encoding via form strings,
   -- implementation-defined and system-specific end of line definitions, and
   -- implementation-defined stray BOMs that we might get from
   -- Ada.Wide_Wide_Text_IO.
   function Read_File (Name             : String;
                       Default_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
                       Actual_Encoding : out Ada.Strings.UTF_Encoding.Encoding_Scheme;
                       Uses_BOM : out Boolean) return Wide_Wide_String;
   
   function Read_File (Name     : String;
                       Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme)
                       return Wide_Wide_String;

end Unicode.IO;
