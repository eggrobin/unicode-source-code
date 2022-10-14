
package Unicode.Source_Code is
   
   type Atom_Kind is (Line_Termination, White_Space, Comment_Content, Numeric,
                      Other);
   
   type Atom_Properties is
      record
         Kind              : Atom_Kind := Other;
         Allows_LRM_Before : Boolean;
      end record;

end Unicode.Source_Code;
