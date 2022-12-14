package Unicode.Properties is
   
   type Binary_Property is
     -- DerivedCoreProperties.txt.
     (Lowercase, Uppercase, Cased, Case_Ignorable, Changes_When_Lowercased,
      Changes_When_Uppercased, Changes_When_Titlecased, Changes_When_Casefolded,
      Changes_When_Casemapped, Alphabetic, Default_Ignorable_Code_Point,
      Grapheme_Base, Grapheme_Extend, Grapheme_Link, Math, Id_Start,
      Id_Continue, XID_Start, XID_Continue,
      -- PropList.txt.
      ASCII_Hex_Digit, Bidi_Control, Dash, Deprecated, Diacritic, Extender,
      Hex_Digit, Hyphen, Ideographic, IDS_Binary_Operator, IDS_Trinary_Operator,
      Join_Control, Logical_Order_Exception, Noncharacter_Code_Point,
      Other_Alphabetic, Other_Default_Ignorable_Code_Point,
      Other_Grapheme_Extend, Other_ID_Continue, Other_ID_Start, Other_Lowercase,
      Other_Math, Other_Uppercase, Pattern_Syntax, Pattern_White_Space,
      Prepended_Concatenation_Mark, Quotation_Mark, Radical, Regional_Indicator,
      Sentence_Terminal, Soft_Dotted, Terminal_Punctuation, Unified_Ideograph,
      Variation_Selector, White_Space,
      -- UnicodeData.txt (but we get it from DerivedBinaryProperties.txt).
      Bidi_Mirrored);
   
   function Get (Property : Binary_Property; C : Code_Point) return Boolean;
   function Set_Of (Property : Binary_Property) return Code_Point_Set;

   type General_Category is (Uppercase_Letter,
                             Lowercase_Letter,
                             Titlecase_Letter,
                             Modifier_Letter,
                             Other_Letter,
                             Nonspacing_Mark,
                             Spacing_Mark,
                             Enclosing_Mark,
                             Decimal_Number,
                             Letter_Number,
                             Other_Number,
                             Connector_Punctuation,
                             Dash_Punctuation,
                             Open_Punctuation,
                             Close_Punctuation,
                             Initial_Punctuation,
                             Final_Punctuation,
                             Other_Punctuation,
                             Math_Symbol,
                             Currency_Symbol,
                             Modifier_Symbol,
                             Other_Symbol,
                             Space_Separator,
                             Line_Separator,
                             Paragraph_Separator,
                             Control,
                             Format,
                             Surrogate,
                             Private_Use,
                             Unassigned);

   package General_Category_Groupings is
      subtype Letter is General_Category range Uppercase_Letter .. Other_Letter;
      subtype Cased_Letter is Letter range Uppercase_Letter .. Titlecase_Letter;
      subtype Mark is General_Category range Nonspacing_Mark .. Enclosing_Mark;
      subtype Punctuation is General_Category range
        Connector_Punctuation .. Other_Punctuation;
      subtype Symbol is General_Category range Math_Symbol .. Other_Symbol;
      subtype Separator is General_Category range
        Space_Separator .. Paragraph_Separator;
      subtype Other is General_Category range Control .. Unassigned;
   end General_Category_Groupings;
   
   
   function Get_General_Category (C : Code_Point) return General_Category;
   function Set_Of (Property : General_Category) return Code_Point_Set;

   function Lu return General_Category renames Uppercase_Letter;
   function Ll return General_Category renames Lowercase_Letter;
   function Lt return General_Category renames Titlecase_Letter;
   function Lm return General_Category renames Modifier_Letter;
   function Lo return General_Category renames Other_Letter;
   
   function Mn return General_Category renames Nonspacing_Mark;
   function Mc return General_Category renames Spacing_Mark;
   function Me return General_Category renames Enclosing_Mark;

   function Nd return General_Category renames Decimal_Number;
   function Nl return General_Category renames Letter_Number;
   function No return General_Category renames Other_Number;

   function Pc return General_Category renames Connector_Punctuation;
   function Pd return General_Category renames Dash_Punctuation;
   function Ps return General_Category renames Open_Punctuation;
   function Pe return General_Category renames Close_Punctuation;
   function Pi return General_Category renames Initial_Punctuation;
   function Pf return General_Category renames Final_Punctuation;
   function Po return General_Category renames Other_Punctuation;

   function Sm return General_Category renames Math_Symbol;
   function Sc return General_Category renames Currency_Symbol;
   function Sk return General_Category renames Modifier_Symbol;
   function So return General_Category renames Other_Symbol;
   
   function Zs return General_Category renames Space_Separator;
   function Zl return General_Category renames Line_Separator;
   function Zp return General_Category renames Paragraph_Separator;

   function Cc return General_Category renames Control;
   function Cf return General_Category renames Format;
   function Cs return General_Category renames Surrogate;
   function Co return General_Category renames Private_Use;
   function Cn return General_Category renames Unassigned;
     
   type Bidi_Class is (-- Strong Types
                       Left_To_Right,
                       Right_To_Left,
                       Arabic_Letter,
                       -- Weak Types
                       European_Number,
                       European_Separator,
                       European_Terminator,
                       Arabic_Number,
                       Common_Separator,
                       Nonspacing_Mark,
                       Boundary_Neutral,
                       -- Neutral Types
                       Paragraph_Separator,
                       Segment_Separator,
                       White_Space,
                       Other_Neutral,
                       -- Explicit Formatting Types
                       Left_To_Right_Embedding,
                       Left_To_Right_Override,
                       Right_To_Left_Embedding,
                       Right_To_Left_Override,
                       Pop_Directional_Format,
                       Left_To_Right_Isolate,
                       Right_To_Left_Isolate,
                       First_Strong_Isolate,
                       Pop_Directional_Isolate);
   
   function Get_Bidi_Class (C : Code_Point) return Bidi_Class;
   function Set_Of (Property : Bidi_Class) return Code_Point_Set;
   
   function L return Bidi_Class renames Left_To_Right;
   function R return Bidi_Class renames Right_To_Left;
   function AL return Bidi_Class renames Arabic_Letter;

   function EN return Bidi_Class renames European_Number;
   function ES return Bidi_Class renames European_Separator;
   function ET return Bidi_Class renames European_Terminator;
   function AN return Bidi_Class renames Arabic_Number;
   function CS return Bidi_Class renames Common_Separator;
   function NSM return Bidi_Class renames Nonspacing_Mark;
   function BN return Bidi_Class renames Boundary_Neutral;

   function B return Bidi_Class renames Paragraph_Separator;
   function S return Bidi_Class renames Segment_Separator;
   function WS return Bidi_Class renames White_Space;
   function ON return Bidi_Class renames Other_Neutral;

   function LRE return Bidi_Class renames Left_To_Right_Embedding;
   function LRO return Bidi_Class renames Left_To_Right_Override;
   function RLE return Bidi_Class renames Right_To_Left_Embedding;
   function RLO return Bidi_Class renames Right_To_Left_Override;
   function PDF return Bidi_Class renames Pop_Directional_Format;
   function LRI return Bidi_Class renames Left_To_Right_Isolate;
   function RLI return Bidi_Class renames Right_To_Left_Isolate;
   function FSI return Bidi_Class renames First_Strong_Isolate;
   function PDI return Bidi_Class renames Pop_Directional_Isolate;
   
end Unicode.Properties;
