foreach (
    $f in @("extracted/DerivedBidiClass",
            "extracted/DerivedGeneralCategory",
            "LineBreak",
            "extracted/DerivedEastAsianWidth",
            "UnicodeData",
            "SpecialCasing",
            "CaseFolding",
            "DerivedCoreProperties",
            "PropList",
            "extracted/DerivedBinaryProperties",
            "emoji/emoji-data")) {
  Invoke-WebRequest "https://www.unicode.org/Public/15.0.0/ucd/$f.txt" -OutFile "15.0.0/$f.txt"
}