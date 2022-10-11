
with Unicode.Properties;

use all type Unicode.Properties.Bidi_Class;
use all type Unicode.Properties.Binary_Property;

package body Unicode.Source_Code.Conversion_To_Plain_Text is
   
   LRM : constant Code_Point := Code_Point'Val (16#200E#);
   function PDF return Code_Point is (Code_Point'Val (16#202C#));
   function FSI return Code_Point is (Code_Point'Val (16#2068#));
   function PDI return Code_Point is (Code_Point'Val (16#2069#));

   function Get_Plain_Text (Converter : Source_Code_Converter)
                            return Wide_Wide_String is
     (To_Wide_Wide_String (Converter.Plain_Text));
   
   procedure Append_Atom (Converter  : in out Source_Code_Converter;
                          Atom       : Wide_Wide_String;
                          Properties : Atom_Properties) is
      New_Atom : Unbounded_Wide_Wide_String :=
        To_Unbounded_Wide_Wide_String (Atom);
   begin
      
      if Atom'Length = 0 then
         return;
      end if;
      
      if Properties.Kind = White_Space then
         declare
            Original : Wide_Wide_String := To_Wide_Wide_String (New_Atom);
         begin
            New_Atom := Null_Unbounded_Wide_Wide_String;
            for C of Original loop
               if not Get (Default_Ignorable_Code_Point, C) then
                  Append (New_Atom, C);
               end if;
            end loop;
         end;
      end if;

      if Converter.Needs_LRM then
         if Properties.Allows_LRM_Before then
            Append (Converter.Plain_Text, LRM);
            Converter.Needs_LRM := False;
         else 
            -- Why isn’t Unbounded_String iterable?
            First_Strong_Or_Number :
            for C of To_Wide_Wide_String (New_Atom) loop
               case Get_Bidi_Class (C) is
                  when R | AL | EN | AN | LRE | RLE | LRI | RLI | FSI =>
                     raise Constraint_Error;
                  when L => exit First_Strong_Or_Number;
                  when others => null;
               end case;
            end loop First_Strong_Or_Number;
         end if;
      end if;
            
      if Properties.Kind = Comment_Content then
         if Element (New_Atom, 1) /= FSI then
            First_Strong :
            for C of To_Wide_Wide_String (New_Atom) loop
               case Get_Bidi_Class (C) is
                  when L => exit First_Strong;
                  when R | AL | LRE | RLE | LRI | RLI | FSI =>
                     New_Atom := FSI & New_Atom;
                     exit First_Strong;
                  when others => null;
               end case;
            end loop First_Strong;
         end if;
      end if;
      
      declare
         Unmatched_Isolates   : Natural := 0;
         Unmatched_Embeddings : Natural := 0;
      begin
         for C of To_Wide_Wide_String (New_Atom) loop
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
         
         if Unmatched_Embeddings + Unmatched_Isolates > 0 then
            if not Properties.At_End_Of_Line then
               if Properties.Kind = Comment_Content then
                  Append (New_Atom,
                          Unmatched_Isolates * PDI &
                            Unmatched_Embeddings * PDF);
               else
                  raise Constraint_Error;
               end if;
            end if;
         end if;
      end;
      
      Last_Strong :
      for C of reverse To_Wide_Wide_String (New_Atom) loop
         case Get_Bidi_Class (C) is
            when L => exit Last_Strong;
            when R | AL | PDF | PDI =>
               Converter.Needs_LRM := True;
               exit Last_Strong;
            when others => null;
         end case;
      end loop Last_Strong;
      
      Append (Converter.Plain_Text, New_Atom);
   end Append_Atom;

end Unicode.Source_Code.Conversion_To_Plain_Text;
