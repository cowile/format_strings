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

   --  Generic formatting functions

   --  Single argument formatter
   generic
      type T is private;
      with function Formatter (Item : T; Spec : Format_Spec) return String;
   function Format (Template : String; Arg : T) return String;

   --  Two argument formatter
   generic
      type T1 is private;
      type T2 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
   function Format_2 (Template : String; Arg1 : T1; Arg2 : T2) return String;

   --  Three argument formatter
   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
      with function Formatter_3 (Item : T3; Spec : Format_Spec) return String;
   function Format_3
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3) return String;

   --  Four argument formatter
   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      type T4 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
      with function Formatter_3 (Item : T3; Spec : Format_Spec) return String;
      with function Formatter_4 (Item : T4; Spec : Format_Spec) return String;
   function Format_4
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3; Arg4 : T4)
      return String;

   --  Five argument formatter
   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      type T4 is private;
      type T5 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
      with function Formatter_3 (Item : T3; Spec : Format_Spec) return String;
      with function Formatter_4 (Item : T4; Spec : Format_Spec) return String;
      with function Formatter_5 (Item : T5; Spec : Format_Spec) return String;
   function Format_5
     (Template : String; Arg1 : T1; Arg2 : T2; Arg3 : T3; Arg4 : T4; Arg5 : T5)
      return String;

   --  Six argument formatter
   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      type T4 is private;
      type T5 is private;
      type T6 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
      with function Formatter_3 (Item : T3; Spec : Format_Spec) return String;
      with function Formatter_4 (Item : T4; Spec : Format_Spec) return String;
      with function Formatter_5 (Item : T5; Spec : Format_Spec) return String;
      with function Formatter_6 (Item : T6; Spec : Format_Spec) return String;
   function Format_6
     (Template : String;
      Arg1     : T1;
      Arg2     : T2;
      Arg3     : T3;
      Arg4     : T4;
      Arg5     : T5;
      Arg6     : T6) return String;

   --  Seven argument formatter
   generic
      type T1 is private;
      type T2 is private;
      type T3 is private;
      type T4 is private;
      type T5 is private;
      type T6 is private;
      type T7 is private;
      with function Formatter_1 (Item : T1; Spec : Format_Spec) return String;
      with function Formatter_2 (Item : T2; Spec : Format_Spec) return String;
      with function Formatter_3 (Item : T3; Spec : Format_Spec) return String;
      with function Formatter_4 (Item : T4; Spec : Format_Spec) return String;
      with function Formatter_5 (Item : T5; Spec : Format_Spec) return String;
      with function Formatter_6 (Item : T6; Spec : Format_Spec) return String;
      with function Formatter_7 (Item : T7; Spec : Format_Spec) return String;
   function Format_7
     (Template : String;
      Arg1     : T1;
      Arg2     : T2;
      Arg3     : T3;
      Arg4     : T4;
      Arg5     : T5;
      Arg6     : T6;
      Arg7     : T7) return String;

   --  Template utilities
   function Count_Holes (Template : String) return Natural;

   --  Helper formatting functions (for backward compatibility)
   function Format_Integer (Value : Integer; Spec : Format_Spec) return String;
   function Format_Float (Value : Float; Spec : Format_Spec) return String;
   function Format_String (Value : String; Spec : Format_Spec) return String;

   --  Backward compatibility overloads
   function Format_Int (Template : String; Arg : Integer) return String;
   function Format_Float (Template : String; Arg : Float) return String;
   function Format_Str (Template : String; Arg : String) return String;
   function Format_2_Int
     (Template : String; Arg1, Arg2 : Integer) return String;

end Format_Strings;
