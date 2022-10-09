with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Strings.UTF_Encoding;
with Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;

use Ada.Wide_Wide_Text_Io;
use Ada.Strings;
use Ada.Strings.Wide_Wide_Fixed;

with Unicode;
with Unicode.IO;
with Unicode.Properties;

use Unicode;

use all type Unicode.Code_Point_Set;
use all type Unicode.Properties.Binary_Property;
use all type Unicode.Properties.General_Category;

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
                             Set_Of (Pattern_Syntax) - To_Set("∂∇∞");

   type Atom_Kind is (Comment_Content, String_Content, White_Space,
                      Line_Terminator,  Syntax, Potential_Identifier);

   type Simple_Atom_Definition is
      record
         Set  : Code_Point_Set;
         Kind : Atom_Kind;
      end record;

   Simple_Atoms           : constant array (Positive range <>) of
     Simple_Atom_Definition :=
       ((White_Space_Atom_Characters, White_Space),
        (Line_Terminators, Line_Terminator),
        (Syntax_Characters, Syntax),
        (not Syntax_Characters and not Line_Terminators and
           not White_Space_Atom_Characters,
         Potential_Identifier));

   Actual_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
   Uses_BOM        : Boolean;

   Text       : Wide_Wide_String :=
                  Unicode.IO.Read_File
                    ("src/unicode.2.ada", Ada.Strings.UTF_Encoding.UTF_8,
                     Actual_Encoding,
                     Uses_BOM);

   procedure Handle_Atom (Kind : Atom_Kind; Atom : Wide_Wide_String) is
   begin
      Put_Line (Kind'Wide_Wide_Image & ": " & Atom);
   end;

   Needs_LRM             : Boolean;
   Position              : Positive := Text'First;
begin
         Put_Line ("");
   while Position <= Text'Last loop
      declare
         Remaining_Text : Wide_Wide_String renames Text (Position .. Text'Last);
         Atom_First     : Positive;
         Atom_Last      : Positive;
         Kind           : Atom_Kind;
      begin
         if Text (Position) = '"' then
            Kind := String_Content;
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
            Handle_Atom (Syntax, "--");
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
               Position := Terminator_Position + 1;
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
         Handle_Atom (Kind, Text (Atom_First .. Atom_Last));
      end;
   end loop;
end Convert_To_Plain_Text;
