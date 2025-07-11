package body Format_Strings.Common is

   --  ========================================================================
   --  UNIFIED FORMAT INTERFACE IMPLEMENTATIONS
   --  ========================================================================
   --  These renaming declarations provide the unified Format interface by
   --  mapping to appropriate generic instantiations

   --  Single argument overloads (4 combinations)
   function Format (Template : String; Arg : String) return String is
      subtype Arg_String is String (Arg'Range);
      function Str_Fmt (Item : Arg_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F is new Format_Strings.Format (Arg_String, Str_Fmt);
   begin
      return F (Template, Arg);
   end Format;

   function Format (Template : String; Arg : Integer) return String is
      function I_Fmt is new Format_Strings.Format (Integer, Integer_Formatter);
   begin
      return I_Fmt (Template, Arg);
   end Format;

   function Format (Template : String; Arg : Float) return String is
      function F_Fmt is new Format_Strings.Format (Float, Float_Formatter);
   begin
      return F_Fmt (Template, Arg);
   end Format;

   function Format (Template : String; Arg : Boolean) return String is
      function B_Fmt is new
        Format_Strings.Format (Boolean, Formatters.Boolean_Formatter);
   begin
      return B_Fmt (Template, Arg);
   end Format;

   --  Two argument overloads (16 combinations)
   function Format (Template : String; Arg1, Arg2 : String) return String is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F2 is new
        Format_2 (Arg1_String, Arg2_String, Str_Fmt1, Str_Fmt2);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Integer, Str_Fmt, Integer_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Float) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Float, Str_Fmt, Float_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Boolean, Str_Fmt, Formatters.Boolean_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Integer, Arg2_String, Integer_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format (Template : String; Arg1, Arg2 : Integer) return String is
      function F2 is new
        Format_2 (Integer, Integer, Integer_Formatter, Integer_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float) return String
   is
      function F2 is new
        Format_2 (Integer, Float, Integer_Formatter, Float_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean) return String
   is
      function F2 is new
        Format_2
          (Integer,
           Boolean,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Float, Arg2_String, Float_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer) return String
   is
      function F2 is new
        Format_2 (Float, Integer, Float_Formatter, Integer_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format (Template : String; Arg1, Arg2 : Float) return String is
      function F2 is new
        Format_2 (Float, Float, Float_Formatter, Float_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean) return String
   is
      function F2 is new
        Format_2
          (Float,
           Boolean,
           Float_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Boolean, Arg2_String, Formatters.Boolean_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer) return String
   is
      function F2 is new
        Format_2
          (Boolean,
           Integer,
           Formatters.Boolean_Formatter,
           Integer_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float) return String
   is
      function F2 is new
        Format_2
          (Boolean,
           Float,
           Formatters.Boolean_Formatter,
           Float_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   function Format (Template : String; Arg1, Arg2 : Boolean) return String is
      function F2 is new
        Format_2
          (Boolean,
           Boolean,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format;

   --  Three argument overloads (64 combinations) - examples for key
   --  combinations For brevity, implementing a few key combinations;
   --  others follow the same pattern

   function Format (Template : String; Arg1, Arg2, Arg3 : String) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Arg1_String,
           Arg2_String,
           Arg3_String,
           Str_Fmt1,
           Str_Fmt2,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Arg1_String,
           Arg2_String,
           Integer,
           Str_Fmt1,
           Str_Fmt2,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2, Arg3 : Integer) return String
   is
      function F3 is new
        Format_3
          (Integer,
           Integer,
           Integer,
           Integer_Formatter,
           Integer_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format (Template : String; Arg1, Arg2, Arg3 : Float) return String
   is
      function F3 is new
        Format_3
          (Float,
           Float,
           Float,
           Float_Formatter,
           Float_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2, Arg3 : Boolean) return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Boolean,
           Boolean,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   --  For remaining three argument combinations, provide stub implementations
   --  These can be implemented following the same pattern as above

   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Float) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Arg1_String,
           Arg2_String,
           Float,
           Str_Fmt1,
           Str_Fmt2,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Boolean) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Arg1_String,
           Arg2_String,
           Boolean,
           Str_Fmt1,
           Str_Fmt2,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : String)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Arg1_String,
           Integer,
           Arg3_String,
           Str_Fmt1,
           Integer_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Integer,
           Integer,
           Str_Fmt1,
           Integer_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Float)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Integer,
           Float,
           Str_Fmt1,
           Integer_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Boolean)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Integer,
           Boolean,
           Str_Fmt1,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : String)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Arg1_String,
           Float,
           Arg3_String,
           Str_Fmt1,
           Float_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Integer)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Float,
           Integer,
           Str_Fmt1,
           Float_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Float) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Float,
           Float,
           Str_Fmt1,
           Float_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Boolean)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Float,
           Boolean,
           Str_Fmt1,
           Float_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : String)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Arg1_String,
           Boolean,
           Arg3_String,
           Str_Fmt1,
           Formatters.Boolean_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Integer)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Boolean,
           Integer,
           Str_Fmt1,
           Formatters.Boolean_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Float)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Boolean,
           Float,
           Str_Fmt1,
           Formatters.Boolean_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Boolean) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function F3 is new
        Format_3
          (Arg1_String,
           Boolean,
           Boolean,
           Str_Fmt1,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Integer,
           Arg2_String,
           Arg3_String,
           Integer_Formatter,
           Str_Fmt2,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Integer)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Integer,
           Arg2_String,
           Integer,
           Integer_Formatter,
           Str_Fmt2,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Float)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Integer,
           Arg2_String,
           Float,
           Integer_Formatter,
           Str_Fmt2,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Boolean)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Integer,
           Arg2_String,
           Boolean,
           Integer_Formatter,
           Str_Fmt2,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : String) return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Integer,
           Integer,
           Arg3_String,
           Integer_Formatter,
           Integer_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : Float) return String
   is
      function F3 is new
        Format_3
          (Integer,
           Integer,
           Float,
           Integer_Formatter,
           Integer_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : Boolean) return String
   is
      function F3 is new
        Format_3
          (Integer,
           Integer,
           Boolean,
           Integer_Formatter,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Integer,
           Float,
           Arg3_String,
           Integer_Formatter,
           Float_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Integer)
      return String
   is
      function F3 is new
        Format_3
          (Integer,
           Float,
           Integer,
           Integer_Formatter,
           Float_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : Float) return String
   is
      function F3 is new
        Format_3
          (Integer,
           Float,
           Float,
           Integer_Formatter,
           Float_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Boolean)
      return String
   is
      function F3 is new
        Format_3
          (Integer,
           Float,
           Boolean,
           Integer_Formatter,
           Float_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Integer,
           Boolean,
           Arg3_String,
           Integer_Formatter,
           Formatters.Boolean_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Integer)
      return String
   is
      function F3 is new
        Format_3
          (Integer,
           Boolean,
           Integer,
           Integer_Formatter,
           Formatters.Boolean_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Float)
      return String
   is
      function F3 is new
        Format_3
          (Integer,
           Boolean,
           Float,
           Integer_Formatter,
           Formatters.Boolean_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : Boolean) return String
   is
      function F3 is new
        Format_3
          (Integer,
           Boolean,
           Boolean,
           Integer_Formatter,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Float,
           Arg2_String,
           Arg3_String,
           Float_Formatter,
           Str_Fmt2,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Integer)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Float,
           Arg2_String,
           Integer,
           Float_Formatter,
           Str_Fmt2,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Float)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Float,
           Arg2_String,
           Float,
           Float_Formatter,
           Str_Fmt2,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Boolean)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Float,
           Arg2_String,
           Boolean,
           Float_Formatter,
           Str_Fmt2,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Float,
           Integer,
           Arg3_String,
           Float_Formatter,
           Integer_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : Integer) return String
   is
      function F3 is new
        Format_3
          (Float,
           Integer,
           Integer,
           Float_Formatter,
           Integer_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Float)
      return String
   is
      function F3 is new
        Format_3
          (Float,
           Integer,
           Float,
           Float_Formatter,
           Integer_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Boolean)
      return String
   is
      function F3 is new
        Format_3
          (Float,
           Integer,
           Boolean,
           Float_Formatter,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : String) return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Float,
           Float,
           Arg3_String,
           Float_Formatter,
           Float_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : Integer) return String
   is
      function F3 is new
        Format_3
          (Float,
           Float,
           Integer,
           Float_Formatter,
           Float_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : Boolean) return String
   is
      function F3 is new
        Format_3
          (Float,
           Float,
           Boolean,
           Float_Formatter,
           Float_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Float,
           Boolean,
           Arg3_String,
           Float_Formatter,
           Formatters.Boolean_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : Integer)
      return String
   is
      function F3 is new
        Format_3
          (Float,
           Boolean,
           Integer,
           Float_Formatter,
           Formatters.Boolean_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : Boolean) return String
   is
      function F3 is new
        Format_3
          (Float,
           Boolean,
           Boolean,
           Float_Formatter,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Boolean,
           Arg2_String,
           Arg3_String,
           Formatters.Boolean_Formatter,
           Str_Fmt2,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Integer)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Boolean,
           Arg2_String,
           Integer,
           Formatters.Boolean_Formatter,
           Str_Fmt2,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Float)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Boolean,
           Arg2_String,
           Float,
           Formatters.Boolean_Formatter,
           Str_Fmt2,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Boolean)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new
        Format_3
          (Boolean,
           Arg2_String,
           Boolean,
           Formatters.Boolean_Formatter,
           Str_Fmt2,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Boolean,
           Integer,
           Arg3_String,
           Formatters.Boolean_Formatter,
           Integer_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : Integer) return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Integer,
           Integer,
           Formatters.Boolean_Formatter,
           Integer_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Float)
      return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Integer,
           Float,
           Formatters.Boolean_Formatter,
           Integer_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Boolean)
      return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Integer,
           Boolean,
           Formatters.Boolean_Formatter,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : String)
      return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Boolean,
           Float,
           Arg3_String,
           Formatters.Boolean_Formatter,
           Float_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Integer)
      return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Float,
           Integer,
           Formatters.Boolean_Formatter,
           Float_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : Float) return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Float,
           Float,
           Formatters.Boolean_Formatter,
           Float_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Boolean)
      return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Float,
           Boolean,
           Formatters.Boolean_Formatter,
           Float_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : String) return String
   is
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String
      is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F3 is new
        Format_3
          (Boolean,
           Boolean,
           Arg3_String,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter,
           Str_Fmt3);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : Integer) return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Boolean,
           Integer,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : Float) return String
   is
      function F3 is new
        Format_3
          (Boolean,
           Boolean,
           Float,
           Formatters.Boolean_Formatter,
           Formatters.Boolean_Formatter,
           Float_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format;

end Format_Strings.Common;
