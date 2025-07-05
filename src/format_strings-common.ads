--  Common pre-instantiated format functions for convenience
--  This package provides ready-to-use format functions for the most
--  common type combinations, eliminating the need for manual instantiation.

with Format_Strings.Formatters;

package Format_Strings.Common is

   --  Instantiate formatters first
   function Integer_Formatter is new Formatters.Integer_Formatter (Integer);
   function Float_Formatter is new Formatters.Float_Formatter (Float);

   --  Single argument convenience functions (these can be instantiated)
   function Format_I is new Format (Integer, Integer_Formatter);
   function Format_F is new Format (Float, Float_Formatter);
   function Format_B is new Format (Boolean, Formatters.Boolean_Formatter);

   --  Single argument wrapper for String
   function Format_S (Template : String; Arg : String) return String;

   --  For String types, we need wrapper functions since String is indefinite

   --  Two argument wrapper for String, Integer
   function Format_SI
     (Template : String; Arg1 : String; Arg2 : Integer) return String;

   --  Two argument wrapper for Integer, String
   function Format_IS
     (Template : String; Arg1 : Integer; Arg2 : String) return String;

   --  Two argument regular instantiations
   function Format_II is new
     Format_2 (Integer, Integer, Integer_Formatter, Integer_Formatter);

   function Format_IF is new
     Format_2 (Integer, Float, Integer_Formatter, Float_Formatter);

   function Format_FI is new
     Format_2 (Float, Integer, Float_Formatter, Integer_Formatter);

   --  Three argument wrappers for combinations with String
   function Format_SII
     (Template : String; Arg1 : String; Arg2, Arg3 : Integer) return String;
   function Format_ISI
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Integer)
      return String;
   function Format_SSI
     (Template : String; Arg1, Arg2 : String; Arg3 : Integer) return String;

   --  Three argument regular instantiation
   function Format_IIF is new
     Format_3
       (Integer,
        Integer,
        Float,
        Integer_Formatter,
        Integer_Formatter,
        Float_Formatter);

   --  Four argument wrappers
   function Format_SSII
     (Template : String; Arg1, Arg2 : String; Arg3, Arg4 : Integer)
      return String;
   function Format_ISIF
     (Template : String;
      Arg1     : Integer;
      Arg2     : String;
      Arg3     : Integer;
      Arg4     : Float) return String;

   --  Five argument wrapper
   function Format_SISIB
     (Template : String;
      Arg1     : String;
      Arg2     : Integer;
      Arg3     : String;
      Arg4     : Integer;
      Arg5     : Boolean) return String;

end Format_Strings.Common;
