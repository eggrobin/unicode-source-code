with Ada.Exceptions;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;

with Unicode.IO;

use Ada.Strings;
use type Ada.Strings.UTF_Encoding.Encoding_Scheme;
use Ada.Strings.Wide_Wide_Fixed;

package body Unicode.Character_Database.Parser is

   type Line_Number is new Positive;
   
   function Get_Line_Terminator_Set return Code_Point_Set is
      Set : Code_Point_Set := Null_Set;
   begin
      -- Ada.Wide_Wide_Characters.Handling should really provide these as sets.
      -- It’s only 2 ** 24 code points, how long can it take?
      for C in Code_Point'Range loop
         if Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (C) then
            Set := Set or To_Set (C);
         end if;
      end loop;
      return Set;
   end Get_Line_Terminator_Set;
      
   -- This should be computed from the line breaking property, but we need to
   -- bootstrap ourselves somehow.  Use the Unicode support built into the
   -- language.
   Line_Terminator : constant Code_Point_Set := Get_Line_Terminator_Set;
     
   function Parse_Code_Point (Hex : Wide_Wide_String) return Code_Point is
     (Code_Point'Val (Natural'Wide_Wide_Value ("16#" & Hex & "#")));
  
   function Parse_Sequence (Field : Wide_Wide_String) return Wide_Wide_String is
      -- When a data field contains a sequence of code points, spaces separate
      -- the code points.
      Length : Natural := (if Field = "" then 0
                           else Count (Field, To_Set (' ')) + 1);
      Result : Wide_Wide_String (1 .. Length);
      First  : Positive;
      Last   : Natural := Field'First - 1;
   begin
      for I in Result'Range loop
         Find_Token
           (Field,
            From => Last + 1,
            Test => Outside, Set => To_Set (' '),
            First => First, Last => Last);
         Result (I) := Parse_Code_Point (Field (First .. Last));
      end loop;
      return Result;
   end Parse_Sequence;
   
   function Parse_Range (Field : Wide_Wide_String) return Code_Point_Range is
      I : constant Natural := Index (Field, "..");
   begin
      if I = 0 then
         return (Low  => Parse_Code_Point (Field),
                 High => Parse_Code_Point (Field));
      else
         return (Low  => Parse_Code_Point (Field (Field'First .. I - 1)),
                 High => Parse_Code_Point (Field (I + 2 .. Field'Last)));
      end if;
   end Parse_Range;
   
   procedure Process_File (File_Name : String) is
      -- 4.2.12: The data files use UTF-8.
      --
      -- However, we can expect files to get reencoded, e.g., if they are piped
      -- through a Windows terminal.  Look for a BOM, default to UTF-8.
      Actual_Encoding : Ada.Strings.UTF_Encoding.Encoding_Scheme;
      Uses_BOM        : Boolean;
      Text            : constant Wide_Wide_String := Unicode.IO.Read_File 
        (File_Name, Ada.Strings.UTF_Encoding.UTF_8, Actual_Encoding, Uses_BOM);
      Next_Line_First : Positive := Text'First;
      Line_First      : Positive;
      Line_Last       : Natural;
      
      Unused          : Integer;
      
      -- 4.2.10: An @missing line starts with the comment character "#",
      -- followed by a space, then the "@missing" keyword, followed by a colon,
      -- another space, a code point range, and a semicolon.
      Missing_Line_Prefix   : constant Wide_Wide_String := "# @missing: ";
   begin
      if Actual_Encoding /= Ada.Strings.UTF_Encoding.UTF_8 or Uses_BOM then
         Ada.Text_IO.Put_Line ("Encoding changed: " & Actual_Encoding'Image &
                               (if Uses_BOM then " with BOM"
                                  else " without BOM"));
      end if;
      
      Lines : for Current_Line_Number in Line_Number loop
         exit when Next_Line_First > Text'Last;
         Line_First := Next_Line_First;
         -- 4.2.13: All data files in the UCD use LF line termination (not
         -- CRLF line termination). When copied to different systems, these
         -- line endings may be automatically changed to use the native line
         -- termination conventions for that system. Make sure your editor
         -- (or parser) can deal with the line termination style in the local
         -- copy of the data files.
         --
         -- Here we deal with anything recognized by UAX #14; we ignore empty
         -- lines, which means we don’t need to special-case CRLF.
         Find_Token (Text,
                     From  => Line_First,
                     Test  => Outside, Set => Line_Terminator,
                     First => Line_First,
                     Last  => Line_Last);
         if Line_Last = 0 then
            Line_Last       := Text'Last;
            Next_Line_First := Line_Last + 1;
         else            Find_Token (Text,
                                     From  => Line_Last,
                                     Test  => Inside, Set => Line_Terminator,
                                     First => Unused,
                                     Last  => Next_Line_First);
            Next_Line_First := Next_Line_First + 1;
         end if;
         if Next_Line_First > Text'Last and then
           Text (Line_First .. Line_Last) /= "# EOF" then
            -- Most of the files have EOF markers, though this is not specified
            -- by UAX #44. Warn if we don’t see such a marker, in case someone
            -- gets a truncated file.
            Ada.Text_IO.Put_Line ("Warning: No EOF marker in " & File_Name);
         end if;
         
         if (for some C of Text (Line_Last + 1 .. Next_Line_First - 1) =>
               C /= Code_Point'Val (16#0A#)) then
            Ada.Text_IO.Put ("Line terminators changed: Line " &
                               Current_Line_Number'Image & ": ");
            Ada.Wide_Wide_Text_IO.Put (Text (Line_First .. Line_Last));
            for C of Text (Line_Last + 1 .. Next_Line_First - 1) loop
               Ada.Text_IO.Put (C'Image);
            end loop;
            Ada.Text_IO.New_Line;
         end if;

         -- 4.2.10: In general, the code point range and semicolon-delimited
         -- list follow the same syntactic conventions as the data file in
         -- which the @missing line occurs, so that any parser which
         -- interprets that data file can easily be adapted to also parse and
         -- interpret an @missing line to pick up default property values for
         -- code points.
         if Text (Line_First .. Line_Last)'Length >=
           Missing_Line_Prefix'Length and then
           Text (Line_First .. Line_First + Missing_Line_Prefix'Length - 1) =
           Missing_Line_Prefix then
            Line_First := Line_First + Missing_Line_Prefix'Length;
         end if;
            
         -- 4.2.4: U+0023 NUMBER SIGN ("#") is used to indicate comments: all
         -- characters from the number sign to the end of the line are
         -- considered part of the comment, and are disregarded when parsing
         -- data.
         Find_Comment : declare
            Comment_First : Natural :=
                              Index (Text (Line_First .. Line_Last),
                                     To_Set ('#'),
                                     From => Line_First);
         begin
            if Comment_First /= 0 then
               Line_Last := Comment_First - 1;
            end if;
         end Find_Comment;

         declare
            Line             : Wide_Wide_String renames
                                 Text (Line_First .. Line_Last);
            Scope            : Code_Point_Range;
            Next_Field_First : Positive := Line'First;
            Field_First      : Positive;
            Field_Last       : Natural;
         begin
            Fields : for Current_Field_Number in Field_Number loop
               exit when Next_Field_First > Line'Last;
               Field_First := Next_Field_First;
               
               -- 4.2.1: Each line of data consists of fields separated by
               -- semicolons.
               declare
                  Separator : Natural := Index (Line, To_Set (';'),
                                                From => Field_First);
               begin
                  if Separator = 0 then 
                     Separator := Line'Last + 1;
                  end if;
                  Field_Last       := Separator - 1;
                  Next_Field_First := Separator + 1;
               end;
                  
               -- 4.2.1: Leading and trailing spaces within a field are not
               -- significant.
               declare
                  Field_With_Spaces : Wide_Wide_String renames 
                    Line (Field_First .. Field_Last);
               begin
                  if Field_With_Spaces /= "" then
                     Field_Last  := Index_Non_Blank (Field_With_Spaces,
                                                     Going => Backward);
                     if Field_Last /= 0 then
                        Field_First := Index_Non_Blank (Field_With_Spaces);
                     end if;
                  end if;
               end;
               declare
                  Field : Wide_Wide_String renames
                            Line (Field_First .. Field_Last);
               begin
                  -- The first field (0) of each line in the Unicode Character
                  -- Database files represents a code point or range. The
                  -- remaining fields (1 .. n) are properties associated with
                  -- that code point.
                  if Current_Field_Number = 0 then
                     if Field = "" then
                        if Next_Field_First <= Line'Last then
                           raise Constraint_Error with
                             "Blank field 0 on nontrivial line";
                        end if;
                     else
                        Scope := Parse_Range (Field);
                     end if;
                  else
                     Process_Field (Scope, Current_Field_Number, Field);
                  end if;
               exception
                  when E : others =>
                     Ada.Exceptions.Raise_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        "Field " & Current_Field_Number'Image & ": " & 
                          Ada.Exceptions.Exception_Message (E));
               end;
            end loop Fields;
         exception
            when E : others =>
               Ada.Exceptions.Raise_Exception
                 (Ada.Exceptions.Exception_Identity (E),
                  "Line " & Current_Line_Number'Image & ": " &
                    Ada.Exceptions.Exception_Message (E));
         end;
      end loop Lines;
   exception
      when E : others =>
         Ada.Exceptions.Raise_Exception
           (Ada.Exceptions.Exception_Identity (E),
            File_Name & ": " & Ada.Exceptions.Exception_Message (E));
   end Process_File;
   
end Unicode.Character_Database.Parser;
