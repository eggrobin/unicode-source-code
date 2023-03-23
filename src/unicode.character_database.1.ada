with Unicode.Properties;
use Unicode.Properties;

package Unicode.Character_Database is

   type Database is tagged private;
   type Access_Database is access Database;   

   function Version (V : Unicode.Version) return Access_Database;
   function Latest return Access_Database;

   function Get (UCD: Database; Property : Binary_Property; C : Code_Point) return Boolean;
   function Set_Of (UCD: Database; Property : Binary_Property) return Code_Point_Set;            
   
   function Get_General_Category (UCD: Database; C : Code_Point) return General_Category;
   function Set_Of (UCD: Database; Property : General_Category) return Code_Point_Set;   
   
   function Get_Bidi_Class (UCD: Database; C : Code_Point) return Bidi_Class;
   function Set_Of (UCD: Database; Property : Bidi_Class) return Code_Point_Set;
   
   function Set_Of (UCD: Database; Property : Line_Break) return Code_Point_Set;
   
   function Set_Of (UCD: Database; Property : East_Asian_Width) return Code_Point_Set;
      
   function Lowercase_Mapping (UCD: Database; C : Code_Point) return Wide_Wide_String;
   function Titlecase_Mapping (UCD: Database; C : Code_Point) return Wide_Wide_String;
   function Uppercase_Mapping (UCD: Database; C : Code_Point) return Wide_Wide_String;
   
   function Case_Folding (UCD: Database; C : Code_Point) return Wide_Wide_String;
   function Simple_Case_Folding (UCD: Database; C : Code_Point) return Code_Point;
   
   function NFKC_Casefold (UCD: Database; C : Code_Point) return Wide_Wide_String;
   function NFKC_SimpleCasefold (UCD: Database; C : Code_Point) return Wide_Wide_String;
   
   function Canonical_Decomposition (UCD: Database; C : Code_Point) return Wide_Wide_String;

   function Get_Canonical_Combining_Class (UCD: Database; C : Code_Point)
                                           return Canonical_Combining_Class;

private
   generic
      type Property is (<>);
   package Enumeration_Property_Data is
      type Values_By_Code_Point is array (Code_Point) of Property;
      type Sets_By_Value is array (Property) of Code_Point_Set;
      type Data is
        record
            Values : Values_By_Code_Point;
            Sets   : Sets_By_Value;
        end record;
   end Enumeration_Property_Data;
   package General_Category_Data is new Enumeration_Property_Data (General_Category);
   package Bidi_Class_Data is new Enumeration_Property_Data (Bidi_Class);
   package Line_Break_Data is new Enumeration_Property_Data (Line_Break);
   package East_Asian_Width_Data is new Enumeration_Property_Data (East_Asian_Width);

   type Binary_Property_Values is array (Code_Point) of Boolean;
   type Binary_Property_Data is
      record
         Values : Binary_Property_Values;
         Set    : Code_Point_Set;
      end record;

   type All_Binary_Property_Data is array (Binary_Property) of Binary_Property_Data;
   
   type Simple_Mapping is array (Code_Point) of Code_Point;

   type Special_Mapping_Value is access Wide_Wide_String;
   type Special_Mapping is array (Code_Point) of Special_Mapping_Value;

   function Identity return Simple_Mapping is ([for C in Code_Point => C]);

   type Database is tagged
      record
         Binary_Properties     : All_Binary_Property_Data;
         General_Categories    : General_Category_Data.Data;
         Bidi_Classes          : Bidi_Class_Data.Data;
         Line_Breaking_Classes : Line_Break_Data.Data;
         East_Asian_Width_Classes : East_Asian_Width_Data.Data;
         Simple_Lowercase_Mapping : Simple_Mapping := Identity;
         Simple_Titlecase_Mapping : Simple_Mapping := Identity;
         Simple_Uppercase_Mapping : Simple_Mapping := Identity;
         Simple_Case_Folding      : Simple_Mapping := Identity;
         Special_Lowercase_Mapping : Special_Mapping := (others => null);
         Special_Titlecase_Mapping : Special_Mapping := (others => null);
         Special_Uppercase_Mapping : Special_Mapping := (others => null);
         Full_Case_Folding         : Special_Mapping := (others => null);
         Canonical_Mapping         : Special_Mapping := (others => null);
         Nontrivial_NFKC_Casefold  : Special_Mapping := (others => null);
         Nontrivial_NFKC_SimpleCasefold  : Special_Mapping := (others => null);
      end record;

end Unicode.Character_Database;
