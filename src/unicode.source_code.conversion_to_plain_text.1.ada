with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

package Unicode.Source_Code.Conversion_To_Plain_Text is
   
   type Source_Code_Converter is private;
   
   function Get_Plain_Text (Converter : Source_Code_Converter)
                            return Wide_Wide_String;
   procedure Append_Atom (Converter  : in out Source_Code_Converter;
                          Atom       : Wide_Wide_String;
                          Properties : Atom_Properties);
   
   
private
   
   type Source_Code_Converter is
      record
         Needs_LRM  : Boolean := False;
         Plain_Text : Unbounded_Wide_Wide_String;
      end record;

end Unicode.Source_Code.Conversion_To_Plain_Text;
