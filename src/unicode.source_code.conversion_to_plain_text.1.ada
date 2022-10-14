with Ada.Strings.Wide_Wide_Unbounded;

package Unicode.Source_Code.Conversion_To_Plain_Text is
   
   type Source_Code_Converter is private;
   
   function Get_Plain_Text (Converter : Source_Code_Converter)
                            return Wide_Wide_String;
   
   type Code_Point_Lookahead (End_Of_File : Boolean) is
      record
         case End_Of_File is
            when False =>
               Next : Code_Point;
            when others => null;
         end case;
      end record;
   
   procedure Append_Atom (Converter  : in out Source_Code_Converter;
                          Atom       : Wide_Wide_String;
                          Lookahead  : Code_Point_Lookahead;
                          Properties : Atom_Properties);
   
private
   
   type Source_Code_Converter is
      record
         Needs_LRM  : Boolean := False;
         Plain_Text : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      end record;

end Unicode.Source_Code.Conversion_To_Plain_Text;
