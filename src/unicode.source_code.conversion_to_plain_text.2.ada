with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Unicode.Properties;

use all type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

use all type Unicode.Properties.Bidi_Class;
use all type Unicode.Properties.Binary_Property;

package body Unicode.Source_Code.Conversion_To_Plain_Text is
   
   LRM : constant Code_Point := Code_Point'Val (16#200E#);
   -- These are functions so that they are overloaded with the Bidi_Class.
   function PDF return Code_Point is (Code_Point'Val (16#202C#));
   function FSI return Code_Point is (Code_Point'Val (16#2068#));
   function PDI return Code_Point is (Code_Point'Val (16#2069#));

   function Get_Plain_Text (Converter : Source_Code_Converter)
                            return Wide_Wide_String is
     (To_Wide_Wide_String (Converter.Plain_Text));
   
   procedure Append_Filtered_Atom (Converter  : in out Source_Code_Converter;
                                   Atom       : Wide_Wide_String;
                                   Properties : Atom_Properties);
   
   procedure Append_Atom (Converter  : in out Source_Code_Converter;
                          Atom       : Wide_Wide_String;
                          Properties : Atom_Properties) is
   begin
      
      if Atom'Length = 0 then
         return;
      end if;
      
      if Properties.Kind = White_Space then
         declare
            package Bounded_By_Atom_Length is
              new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length
                (Atom'Length);
            Filtered_Atom : Bounded_By_Atom_Length.Bounded_Wide_Wide_String;
         begin
            for C of Atom loop
               if not Get (Default_Ignorable_Code_Point, C) then
                  Bounded_By_Atom_Length.Append (Filtered_Atom, C);
               end if;
            end loop;
            Append_Filtered_Atom
              (Converter,
               Bounded_By_Atom_Length.To_Wide_Wide_String (Filtered_Atom),
               Properties);
         end;
      else
         Append_Filtered_Atom (Converter, Atom, Properties);
      end if;
   end Append_Atom;
   
   
   procedure Append_Filtered_Atom (Converter  : in out Source_Code_Converter;
                                   Atom       : Wide_Wide_String;
                                   Properties : Atom_Properties) is
      Prefix_FSI : Boolean := False;
   begin

      if Converter.Needs_LRM then
         if Properties.Allows_LRM_Before then
            Append (Converter.Plain_Text, LRM);
            Converter.Needs_LRM := False;
         else 
            First_Strong_Or_Number_Or_Explicit :
            for C of Atom loop
               case Get_Bidi_Class (C) is
                  when R | AL | EN | AN | LRE | RLE | LRI | RLI | FSI =>
                     raise Constraint_Error;
                  when L => exit First_Strong_Or_Number_Or_Explicit;
                  when others => null;
               end case;
            end loop First_Strong_Or_Number_Or_Explicit;
         end if;
      end if;
            
      if Properties.Kind = Comment_Content and then
        Atom (Atom'First) /= FSI then
         First_Strong_Or_Explicit :
         for C of Atom loop
            case Get_Bidi_Class (C) is
               when L => exit First_Strong_Or_Explicit;
               when R | AL | LRE | RLE | LRI | RLI | FSI =>
                  Prefix_FSI := True;
                  exit First_Strong_Or_Explicit;
               when others => null;
            end case;
         end loop First_Strong_Or_Explicit;
      end if;
      
      if Prefix_FSI then
         Append (Converter.Plain_Text, FSI);
      end if;
      Append (Converter.Plain_Text, Atom);
      
      declare
         Unmatched_Isolates   : Natural := (if Prefix_FSI then 1 else 0);
         Unmatched_Embeddings : Natural := 0;
      begin
         for C of Atom loop
            case Get_Bidi_Class (C) is
               when LRI | RLI | FSI =>
                  Unmatched_Isolates := Unmatched_Isolates + 1;
               when PDI =>
                  if Unmatched_Isolates > 0 then
                     Unmatched_Isolates := Unmatched_Isolates - 1;
                  end if;
               when LRE | RLE | LRO | RLO =>
                  if Unmatched_Isolates = 0 then
                     Unmatched_Embeddings := Unmatched_Embeddings + 1;
                  end if;
               when PDF =>
                  if Unmatched_Isolates = 0 and then
                    Unmatched_Embeddings > 0 then
                     Unmatched_Embeddings := Unmatched_Embeddings - 1;
                  end if;
               when others => null;
            end case;
         end loop;
         
         if not Properties.At_End_Of_Line then 
            if Unmatched_Embeddings + Unmatched_Isolates > 0 then
               if Properties.Kind = Comment_Content then
                  Append (Converter.Plain_Text,
                          Unmatched_Isolates * PDI &
                            Unmatched_Embeddings * PDF);
               else
                  raise Constraint_Error;
               end if;
               Converter.Needs_LRM := True;
            else
               Last_Strong_Or_Explicit :
               for C of reverse Atom loop
                  case Get_Bidi_Class (C) is
                     when L => exit Last_Strong_Or_Explicit;
                     when R | AL | PDF | PDI =>
                        Converter.Needs_LRM := True;
                        exit Last_Strong_Or_Explicit;
                     when others => null;
                  end case;
               end loop Last_Strong_Or_Explicit;
            end if;
         end if;
      end;
   end Append_Filtered_Atom;

end Unicode.Source_Code.Conversion_To_Plain_Text;
