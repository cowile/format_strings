--  Format_Strings: Type-safe string formatting for Ada
--
--  This library provides printf-like formatting with compile-time safety
--  and an ergonomic API designed for modern Ada development.

package Format_Strings is

   Format_Error : exception;

   --  Types for format specifications
   type Alignment_Type is (Left, Right, Center, Default);

   type Format_Spec is record
      --  Alignment and padding
      Alignment : Alignment_Type := Default;
      Fill_Char : Character := ' ';

      --  Width and precision
      Width         : Natural := 0;  -- 0 means no minimum width
      Precision     : Natural := 0;  -- 0 means default precision
      Has_Precision : Boolean := False;

      --  Type specifier
      Type_Char : Character := ' ';  -- ' ' means default for type

      --  Flags
      Show_Sign : Boolean := False;  -- '+' flag
      Alt_Form  : Boolean := False;  -- '#' flag
      Zero_Pad  : Boolean := False;  -- '0' flag
   end record;

   --  Parse format specifier from string (e.g. ">10.2f")
   function Parse_Spec (Spec_String : String) return Format_Spec;

   --  Basic formatting functions
   function Format (Template : String; Arg : Integer) return String;
   function Format (Template : String; Arg : Float) return String;
   function Format (Template : String; Arg : String) return String;
   function Format (Template : String; Arg1, Arg2 : Integer) return String;

   --  Template utilities
   function Count_Holes (Template : String) return Natural;

end Format_Strings;
