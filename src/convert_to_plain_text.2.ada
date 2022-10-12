with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;

use Ada.Wide_Wide_Text_Io;
use Ada.Strings;
use Ada.Strings.Wide_Wide_Fixed;
use Ada.Strings.Wide_Wide_Unbounded;

with Unicode;
with Unicode.IO;
with Unicode.Properties;
with Unicode.Source_Code.Conversion_To_Plain_Text;

use Unicode;

use all type Unicode.Code_Point_Set;
use all type Unicode.Properties.Binary_Property;
use all type Unicode.Properties.General_Category;
use all type Unicode.Source_Code.Atom_Kind;
use all type Unicode.Source_Code.Conversion_To_Plain_Text.Source_Code_Converter;

procedure Convert_To_Plain_Text is

   -- 2.2 (13/3) and 2/3, with no additional implementation-defined
   -- representations for an end of line.
   Line_Terminators : constant Code_Point_Set :=
     To_Set (Sequence =>
               (Code_Point'Val (16#0A#), Code_Point'Val (16#0B#),
                Code_Point'Val (16#0C#), Code_Point'Val (16#0D#),
                Code_Point'Val (16#85#))) or
     Set_Of (Line_Separator) or Set_Of (Paragraph_Separator);

   -- 2.3 (3/2 .. 5/3). The end of the line is treated separately.
   White_Space_Characters : constant Code_Point_Set :=
     Set_Of (Space_Separator) or
     To_Set (Code_Point'Val (16#09#));

   -- 2.3 (7.1/3).
   White_Space_Atom_Characters : constant Code_Point_Set :=
     White_Space_Characters or Set_Of (Format);

   Syntax_Characters     : constant Code_Point_Set :=
     Set_Of (Pattern_Syntax) - To_Set ("∂∇∞");

   type Simple_Atom_Definition is
      record
         Set  : Code_Point_Set;
         Kind : Unicode.Source_Code.Atom_Kind;
      end record;

   Simple_Atoms           : constant array (Positive range <>) of
     Simple_Atom_Definition :=
       ((White_Space_Atom_Characters, White_Space),
        (Line_Terminators, Line_Termination),
        (Syntax_Characters - To_Set ("'-"""), Other),
        (not Syntax_Characters and not Line_Terminators and
               not White_Space_Atom_Characters,
         Other));

   Actual_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
   Uses_BOM        : Boolean;

   Text       : Wide_Wide_String :=
                  Unicode.IO.Read_File
                    ("src/test.2.ada", Ada.Strings.UTF_Encoding.UTF_8,
                     Actual_Encoding,
                     Uses_BOM);

   Converter : Source_Code.Conversion_To_Plain_Text.Source_Code_Converter;

   Position : Positive := Text'First;
   function Is_Line_Last (I : Positive) return Boolean is
     (I = Text'Last or else
      Is_In (Text (I + 1), Line_Terminators));
begin
   while Position <= Text'Last loop
      declare
         Remaining_Text : Wide_Wide_String renames Text (Position .. Text'Last);
      begin
         case Text (Position) is
            when '"' =>
               Append_Atom (Converter, """",
                            (Other,
                             Is_Line_Last (Position),
                             Allows_LRM_Before => True));
               Position := Position + 1;
               for I in Positive range Position .. Text'Last + 1 loop
                  if Is_Line_Last (I) then
                     -- Unterminated string literal.
                     Append_Atom (Converter, Text (Position .. I),
                                  (Other,
                                   At_End_Of_Line    => True,
                                   Allows_LRM_Before => False));
                     exit;
                  elsif Text (I) = '"' and then
                    (I = Text'Last or else Text (I + 1) /= '"') then
                     Append_Atom (Converter, Text (Position .. I - 1),
                                  (Other,
                                   At_End_Of_Line    => False,
                                   Allows_LRM_Before => False));
                     Position := I + 1;
                     Append_Atom (Converter, """",
                                  (Other,
                                   Is_Line_Last (Position),
                                   Allows_LRM_Before => False));
                     exit;
                  end if;
               end loop;
            when ''' =>
               if Remaining_Text'Length >= 3 and then
                 Text (Position + 2) = ''' then
                  declare
                     Literal : constant Wide_Wide_Character :=
                       Text (Position + 1);
                  begin
                     Append_Atom (Converter, "'",
                                  (Other,
                                   At_End_Of_Line => False,
                                   Allows_LRM_Before => True));
                     Append_Atom (Converter, (1 => Literal),
                                  (Other,
                                   At_End_Of_Line => False,
                                   Allows_LRM_Before => False));
                     Append_Atom (Converter, "'",
                                  (Other,
                                   Is_Line_Last (Position + 2),
                                   Allows_LRM_Before => False));
                  end;
                  Position := Position + 3;
               else
                  Append_Atom (Converter, "'",
                               (Other,
                                Is_Line_Last (Position),
                                Allows_LRM_Before => True));
                  Position := Position + 1;
               end if;
            when '-' =>
               if Remaining_Text'Length >= 2 and then
                 Text (Position + 1) = '-' then
                  Append_Atom (Converter, "--",
                               (Other,
                                Is_Line_Last (Position + 1),
                                Allows_LRM_Before => True));
                  Position := Position + 2;
                  declare
                     End_Of_Line : constant Natural :=
                       Index (Text, From => Position,
                              Test       => Inside,
                              Set        => Line_Terminators);
                     Comment_Last : constant Positive :=
                       (if End_Of_Line = 0 then Text'Last
                        else End_Of_Line - 1);
                  begin
                     Append_Atom (Converter, Text (Position .. Comment_Last),
                                  (Comment_Content,
                                   At_End_Of_Line    => True,
                                   Allows_LRM_Before => True));
                     Position := Comment_Last + 1;
                  end;
               else
                  Append_Atom (Converter, "-",
                               (Other,
                                Is_Line_Last (Position),
                                Allows_LRM_Before => True));
                  Position := Position + 1;
               end if;
            when others =>
               for Definition of Simple_Atoms loop
                  if Is_In (Text (Position), Definition.Set) then
                     declare
                        Next_Position : constant Natural :=
                          Index (Remaining_Text,
                                 Test => Outside,
                                 Set  => Definition.Set);
                        Atom_Last     : constant Positive :=
                          (if Next_Position = 0 then Text'Last
                           else Next_Position - 1);
                     begin
                        -- Note that an LRM is not always allowed here.  For
                        -- instance, we may be in the middle of a numeric
                        -- literal, and we do not lex those.  However, this does
                        -- not matter for this algorithm: whenever a strongly
                        -- right-to-left character occurs, we always correctly
                        -- identify the first opportunity to insert an LRM.
                        Append_Atom (Converter, Text (Position .. Atom_Last),
                                     (Definition.Kind,
                                        Is_Line_Last (Atom_Last),
                                        Allows_LRM_Before => True));
                        Position := Atom_Last + 1;
                     end;
                     exit;
                  end if;
               end loop;
         end case;
      end;
   end loop;
   declare
      Plain_Text   : constant Wide_Wide_String := Get_Plain_Text (Converter);
   begin
      Unicode.IO.Write_File (Plain_Text, "src/test.fixed.2.ada",
                             Actual_Encoding, Uses_BOM);
   end;
end Convert_To_Plain_Text;
