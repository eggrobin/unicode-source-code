with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Strings.UTF_Encoding;
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

   -- 2.3 (3/2).
   Identifier_Start : constant Code_Point_Set :=
     Set_Of (Uppercase_Letter) or
     Set_Of (Lowercase_Letter) or
     Set_Of (Titlecase_Letter) or
     Set_Of (Modifier_Letter) or
     Set_Of (Other_Letter) or
     Set_Of (Letter_Number);
   -- 2.3 (3.1/3).
   Identifier_Extend : constant Code_Point_Set :=
     Set_Of (Nonspacing_Mark) or
     Set_Of (Spacing_Mark) or
     Set_Of (Decimal_Number) or
     Set_Of (Connector_Punctuation);

   Identifier_Characters : constant Code_Point_Set :=
     Identifier_Start or Identifier_Extend;

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
        (Syntax_Characters, Other),
        (not Syntax_Characters and not Line_Terminators and
               not White_Space_Atom_Characters,
         Other));

   Actual_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
   Uses_BOM        : Boolean;

   Text       : Wide_Wide_String :=
                  Unicode.IO.Read_File
                    ("src/unicode.character_database.2.ada", Ada.Strings.UTF_Encoding.UTF_8,
                     Actual_Encoding,
                     Uses_BOM);

   Converter : Source_Code.Conversion_To_Plain_Text.Source_Code_Converter;

   Position : Positive := Text'First;
begin
   while Position <= Text'Last loop
      declare
         Remaining_Text : Wide_Wide_String renames Text (Position .. Text'Last);
         Atom_First     : Positive;
         Atom_Last      : Positive;
         Kind           : Unicode.Source_Code.Atom_Kind;
      begin
         if Text (Position) = ''' and then Position + 2 <= Text'Last and then
           Text (Position + 2) = ''' then
            Kind := Other;
            Atom_First := Position;
            Atom_Last := Position + 2;
            Position := Position + 3;
         elsif Text (Position) = '"' then
            Kind := Other;
            Atom_First := Position;
            declare
               Terminator_Position : Natural := Atom_First;
            begin
               loop
                  Terminator_Position :=
                    Index (Remaining_Text, To_Set ('"') or Line_Terminators,
                           From => Terminator_Position);
                  if Terminator_Position = 0 then
                     Terminator_Position := Text'Last + 1;
                  end if;
                  exit when Terminator_Position >= Text'Last or else
                    Text (Terminator_Position ..
                            Terminator_Position + 1) /= """""";
                  Terminator_Position := Terminator_Position + 1;
               end loop;
               Atom_Last := Terminator_Position;
               Position := Terminator_Position + 1;
            end;
         elsif Remaining_Text'Length >= 2 and then
           Remaining_Text (Position .. Position + 1) = "--" then
            Append_Atom (Converter, "--",
                         (Other,
                          At_End_Of_Line    => False,
                          Allows_Lrm_Before => True));
            Kind := Comment_Content;
            Atom_First := Position + 2;
            declare
               Terminator_Position : Natural := Atom_First;
            begin
               Terminator_Position := Index (Remaining_Text,
                                             Line_Terminators,
                                             From => Terminator_Position);
               if Terminator_Position = 0 then
                  Terminator_Position := Text'Last + 1;
               end if;
               Atom_Last := Terminator_Position - 1;
               Position := Terminator_Position;
            end;
         else
            for Definition of Simple_Atoms loop
               if Is_In (Text (Position), Definition.Set) then
                  Kind := Definition.Kind;
                  Atom_First := Position;
                  declare
                     Next_Position : constant Natural :=
                                       Index (Remaining_Text,
                                              Test => Outside,
                                              Set  => Definition.Set);
                  begin
                     Position := (if Next_Position = 0 then Text'Last + 1
                                  else Next_Position);
                  end;
                  Atom_Last := Position - 1;
                  exit;
               end if;
            end loop;
         end if;
         Append_Atom (Converter, Text (Atom_First .. Atom_Last),
                      (Kind,
                       At_End_Of_Line    => Position > Text'Last or else
                       Is_In (Text (Position), Line_Terminators),
                       Allows_LRM_Before => True));
      end;
   end loop;
   Put_Line (Text'Length'Wide_Wide_Image);
   Put_Line (Get_Plain_Text (Converter)'Length'Wide_Wide_Image);
end Convert_To_Plain_Text;
