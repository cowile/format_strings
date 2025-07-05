
package body Format_Strings.Common is

   --  Helper type for string wrapper
   type String_Holder (Length : Natural) is record
      Value : String (1 .. Length);
   end record;

   --  Two argument wrapper for String, Integer
   function Format_SI
     (Template : String; Arg1 : String; Arg2 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new Format_2 (Arg1_String, Integer, Str_Fmt, Integer_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_SI;

   --  Two argument wrapper for Integer, String
   function Format_IS
     (Template : String; Arg1 : Integer; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new Format_2 (Integer, Arg2_String, Integer_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_IS;

   --  Three argument wrappers
   function Format_SII
     (Template : String; Arg1 : String; Arg2, Arg3 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F3 is new Format_3
        (Arg1_String, Integer, Integer, Str_Fmt, Integer_Formatter, Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format_SII;

   function Format_ISI
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Integer)
      return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F3 is new Format_3
        (Integer, Arg2_String, Integer, Integer_Formatter, Str_Fmt, Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format_ISI;

   function Format_SSI
     (Template : String; Arg1, Arg2 : String; Arg3 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F3 is new Format_3
        (Arg1_String, Arg2_String, Integer, Str_Fmt1, Str_Fmt2, Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format_SSI;

   --  Four argument wrappers
   function Format_SSII
     (Template : String; Arg1, Arg2 : String; Arg3, Arg4 : Integer)
      return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt2 (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt2;
      function F4 is new Format_4
        (Arg1_String, Arg2_String, Integer, Integer,
         Str_Fmt1, Str_Fmt2, Integer_Formatter, Integer_Formatter);
   begin
      return F4 (Template, Arg1, Arg2, Arg3, Arg4);
   end Format_SSII;

   function Format_ISIF
     (Template : String;
      Arg1     : Integer;
      Arg2     : String;
      Arg3     : Integer;
      Arg4     : Float) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F4 is new Format_4
        (Integer, Arg2_String, Integer, Float,
         Integer_Formatter, Str_Fmt, Integer_Formatter, Float_Formatter);
   begin
      return F4 (Template, Arg1, Arg2, Arg3, Arg4);
   end Format_ISIF;

   --  Five argument wrapper
   function Format_SISIB
     (Template : String;
      Arg1     : String;
      Arg2     : Integer;
      Arg3     : String;
      Arg4     : Integer;
      Arg5     : Boolean) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      subtype Arg3_String is String (Arg3'Range);
      function Str_Fmt1 (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt1;
      function Str_Fmt3 (Item : Arg3_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt3;
      function F5 is new Format_5
        (Arg1_String, Integer, Arg3_String, Integer, Boolean,
         Str_Fmt1, Integer_Formatter, Str_Fmt3, Integer_Formatter,
         Formatters.Boolean_Formatter);
   begin
      return F5 (Template, Arg1, Arg2, Arg3, Arg4, Arg5);
   end Format_SISIB;

end Format_Strings.Common;