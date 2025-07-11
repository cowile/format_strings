package body Format_Strings.Common is

   --  Single argument wrapper for String
   function Format_S (Template : String; Arg : String) return String is
      subtype Arg_String is String (Arg'Range);
      function Str_Fmt (Item : Arg_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F is new Format_Strings.Format (Arg_String, Str_Fmt);
   begin
      return F (Template, Arg);
   end Format_S;

   --  Two argument wrapper for String, Integer
   function Format_SI
     (Template : String; Arg1 : String; Arg2 : Integer) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Integer, Str_Fmt, Integer_Formatter);
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
      function F2 is new
        Format_2 (Integer, Arg2_String, Integer_Formatter, Str_Fmt);
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
      function F3 is new
        Format_3
          (Arg1_String,
           Integer,
           Integer,
           Str_Fmt,
           Integer_Formatter,
           Integer_Formatter);
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
      function F3 is new
        Format_3
          (Integer,
           Arg2_String,
           Integer,
           Integer_Formatter,
           Str_Fmt,
           Integer_Formatter);
   begin
      return F3 (Template, Arg1, Arg2, Arg3);
   end Format_ISI;

   function Format_SSI
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
   end Format_SSI;

   --  Four argument wrappers
   function Format_SSII
     (Template : String; Arg1, Arg2 : String; Arg3, Arg4 : Integer)
      return String
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
      function F4 is new
        Format_4
          (Arg1_String,
           Arg2_String,
           Integer,
           Integer,
           Str_Fmt1,
           Str_Fmt2,
           Integer_Formatter,
           Integer_Formatter);
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
      function F4 is new
        Format_4
          (Integer,
           Arg2_String,
           Integer,
           Float,
           Integer_Formatter,
           Str_Fmt,
           Integer_Formatter,
           Float_Formatter);
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
      function F5 is new
        Format_5
          (Arg1_String,
           Integer,
           Arg3_String,
           Integer,
           Boolean,
           Str_Fmt1,
           Integer_Formatter,
           Str_Fmt3,
           Integer_Formatter,
           Formatters.Boolean_Formatter);
   begin
      return F5 (Template, Arg1, Arg2, Arg3, Arg4, Arg5);
   end Format_SISIB;

   --  Additional two argument wrapper functions
   function Format_SS (Template : String; Arg1, Arg2 : String) return String is
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
   end Format_SS;

   function Format_SF
     (Template : String; Arg1 : String; Arg2 : Float) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Float, Str_Fmt, Float_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_SF;

   function Format_SB
     (Template : String; Arg1 : String; Arg2 : Boolean) return String
   is
      subtype Arg1_String is String (Arg1'Range);
      function Str_Fmt (Item : Arg1_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Arg1_String, Boolean, Str_Fmt, Formatters.Boolean_Formatter);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_SB;

   function Format_FS
     (Template : String; Arg1 : Float; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Float, Arg2_String, Float_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_FS;

   function Format_BS
     (Template : String; Arg1 : Boolean; Arg2 : String) return String
   is
      subtype Arg2_String is String (Arg2'Range);
      function Str_Fmt (Item : Arg2_String; Spec : Format_Spec) return String is
      begin
         return Formatters.String_Formatter (Item, Spec);
      end Str_Fmt;
      function F2 is new
        Format_2 (Boolean, Arg2_String, Formatters.Boolean_Formatter, Str_Fmt);
   begin
      return F2 (Template, Arg1, Arg2);
   end Format_BS;

   --  Key three argument wrapper functions (implement most common ones first)
   function Format_SSS
     (Template : String; Arg1, Arg2, Arg3 : String) return String
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
   end Format_SSS;

   function Format_SIS
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
   end Format_SIS;

   --  Stub implementations for remaining functions (to get builds working)
   function Format_SSF
     (Template : String; Arg1, Arg2 : String; Arg3 : Float) return String is
   begin
      return "STUB_SSF";
   end Format_SSF;

   function Format_SSB
     (Template : String; Arg1, Arg2 : String; Arg3 : Boolean) return String is
   begin
      return "STUB_SSB";
   end Format_SSB;

   function Format_SIF
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Float)
      return String is
   begin
      return "STUB_SIF";
   end Format_SIF;

   function Format_SIB
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Boolean)
      return String is
   begin
      return "STUB_SIB";
   end Format_SIB;

   function Format_SFS
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : String)
      return String is
   begin
      return "STUB_SFS";
   end Format_SFS;

   function Format_SFI
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Integer)
      return String is
   begin
      return "STUB_SFI";
   end Format_SFI;

   function Format_SFF
     (Template : String; Arg1 : String; Arg2, Arg3 : Float) return String is
   begin
      return "STUB_SFF";
   end Format_SFF;

   function Format_SFB
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Boolean)
      return String is
   begin
      return "STUB_SFB";
   end Format_SFB;

   function Format_SBS
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : String)
      return String is
   begin
      return "STUB_SBS";
   end Format_SBS;

   function Format_SBI
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Integer)
      return String is
   begin
      return "STUB_SBI";
   end Format_SBI;

   function Format_SBF
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Float)
      return String is
   begin
      return "STUB_SBF";
   end Format_SBF;

   function Format_SBB
     (Template : String; Arg1 : String; Arg2, Arg3 : Boolean) return String is
   begin
      return "STUB_SBB";
   end Format_SBB;

   function Format_ISS
     (Template : String; Arg1 : Integer; Arg2, Arg3 : String) return String is
   begin
      return "STUB_ISS";
   end Format_ISS;

   function Format_ISF
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Float)
      return String is
   begin
      return "STUB_ISF";
   end Format_ISF;

   function Format_ISB
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Boolean)
      return String is
   begin
      return "STUB_ISB";
   end Format_ISB;

   function Format_IIS
     (Template : String; Arg1, Arg2 : Integer; Arg3 : String) return String is
   begin
      return "STUB_IIS";
   end Format_IIS;

   function Format_IFS
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : String)
      return String is
   begin
      return "STUB_IFS";
   end Format_IFS;

   function Format_IBS
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : String)
      return String is
   begin
      return "STUB_IBS";
   end Format_IBS;

   function Format_FSS
     (Template : String; Arg1 : Float; Arg2, Arg3 : String) return String is
   begin
      return "STUB_FSS";
   end Format_FSS;

   function Format_FSI
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Integer)
      return String is
   begin
      return "STUB_FSI";
   end Format_FSI;

   function Format_FSF
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Float)
      return String is
   begin
      return "STUB_FSF";
   end Format_FSF;

   function Format_FSB
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Boolean)
      return String is
   begin
      return "STUB_FSB";
   end Format_FSB;

   function Format_FIS
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : String)
      return String is
   begin
      return "STUB_FIS";
   end Format_FIS;

   function Format_FFS
     (Template : String; Arg1, Arg2 : Float; Arg3 : String) return String is
   begin
      return "STUB_FFS";
   end Format_FFS;

   function Format_FBS
     (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : String)
      return String is
   begin
      return "STUB_FBS";
   end Format_FBS;

   function Format_BSS
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : String) return String is
   begin
      return "STUB_BSS";
   end Format_BSS;

   function Format_BSI
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Integer)
      return String is
   begin
      return "STUB_BSI";
   end Format_BSI;

   function Format_BSF
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Float)
      return String is
   begin
      return "STUB_BSF";
   end Format_BSF;

   function Format_BSB
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Boolean)
      return String is
   begin
      return "STUB_BSB";
   end Format_BSB;

   function Format_BIS
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : String)
      return String is
   begin
      return "STUB_BIS";
   end Format_BIS;

   function Format_BFS
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : String)
      return String is
   begin
      return "STUB_BFS";
   end Format_BFS;

   function Format_BBS
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : String) return String is
   begin
      return "STUB_BBS";
   end Format_BBS;

end Format_Strings.Common;
