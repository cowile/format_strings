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

   --  Complete two argument instantiations (16 total: 4^2)
   function Format_SS (Template : String; Arg1, Arg2 : String) return String;
   function Format_SF (Template : String; Arg1 : String; Arg2 : Float) return String;
   function Format_SB (Template : String; Arg1 : String; Arg2 : Boolean) return String;
   function Format_FS (Template : String; Arg1 : Float; Arg2 : String) return String;
   function Format_FF is new Format_2 (Float, Float, Float_Formatter, Float_Formatter);
   function Format_FB is new Format_2 (Float, Boolean, Float_Formatter, Formatters.Boolean_Formatter);
   function Format_BS (Template : String; Arg1 : Boolean; Arg2 : String) return String;
   function Format_BI is new Format_2 (Boolean, Integer, Formatters.Boolean_Formatter, Integer_Formatter);
   function Format_BF is new Format_2 (Boolean, Float, Formatters.Boolean_Formatter, Float_Formatter);
   function Format_BB is new Format_2 (Boolean, Boolean, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter);
   function Format_IB is new Format_2 (Integer, Boolean, Integer_Formatter, Formatters.Boolean_Formatter);

   --  Complete three argument functions (64 total: 4^3)
   --  All SSX combinations
   function Format_SSS (Template : String; Arg1, Arg2, Arg3 : String) return String;
   function Format_SSF (Template : String; Arg1, Arg2 : String; Arg3 : Float) return String;
   function Format_SSB (Template : String; Arg1, Arg2 : String; Arg3 : Boolean) return String;
   
   --  All SIX combinations  
   function Format_SIS (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : String) return String;
   function Format_SIF (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Float) return String;
   function Format_SIB (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Boolean) return String;
   
   --  All SFX combinations
   function Format_SFS (Template : String; Arg1 : String; Arg2 : Float; Arg3 : String) return String;
   function Format_SFI (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Integer) return String;
   function Format_SFF (Template : String; Arg1 : String; Arg2, Arg3 : Float) return String;
   function Format_SFB (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Boolean) return String;
   
   --  All SBX combinations
   function Format_SBS (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : String) return String;
   function Format_SBI (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Integer) return String;
   function Format_SBF (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Float) return String;
   function Format_SBB (Template : String; Arg1 : String; Arg2, Arg3 : Boolean) return String;
   
   --  All ISX combinations
   function Format_ISS (Template : String; Arg1 : Integer; Arg2, Arg3 : String) return String;
   function Format_ISF (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Float) return String;
   function Format_ISB (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Boolean) return String;
   
   --  All IIX combinations
   function Format_IIS (Template : String; Arg1, Arg2 : Integer; Arg3 : String) return String;
   function Format_III is new Format_3 (Integer, Integer, Integer, Integer_Formatter, Integer_Formatter, Integer_Formatter);
   function Format_IIB is new Format_3 (Integer, Integer, Boolean, Integer_Formatter, Integer_Formatter, Formatters.Boolean_Formatter);
   
   --  All IFX combinations
   function Format_IFS (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : String) return String;
   function Format_IFI is new Format_3 (Integer, Float, Integer, Integer_Formatter, Float_Formatter, Integer_Formatter);
   function Format_IFF is new Format_3 (Integer, Float, Float, Integer_Formatter, Float_Formatter, Float_Formatter);
   function Format_IFB is new Format_3 (Integer, Float, Boolean, Integer_Formatter, Float_Formatter, Formatters.Boolean_Formatter);
   
   --  All IBX combinations
   function Format_IBS (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : String) return String;
   function Format_IBI is new Format_3 (Integer, Boolean, Integer, Integer_Formatter, Formatters.Boolean_Formatter, Integer_Formatter);
   function Format_IBF is new Format_3 (Integer, Boolean, Float, Integer_Formatter, Formatters.Boolean_Formatter, Float_Formatter);
   function Format_IBB is new Format_3 (Integer, Boolean, Boolean, Integer_Formatter, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter);
   
   --  All FSX combinations
   function Format_FSS (Template : String; Arg1 : Float; Arg2, Arg3 : String) return String;
   function Format_FSI (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Integer) return String;
   function Format_FSF (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Float) return String;
   function Format_FSB (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Boolean) return String;
   
   --  All FIX combinations
   function Format_FIS (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : String) return String;
   function Format_FII is new Format_3 (Float, Integer, Integer, Float_Formatter, Integer_Formatter, Integer_Formatter);
   function Format_FIF is new Format_3 (Float, Integer, Float, Float_Formatter, Integer_Formatter, Float_Formatter);
   function Format_FIB is new Format_3 (Float, Integer, Boolean, Float_Formatter, Integer_Formatter, Formatters.Boolean_Formatter);
   
   --  All FFX combinations
   function Format_FFS (Template : String; Arg1, Arg2 : Float; Arg3 : String) return String;
   function Format_FFI is new Format_3 (Float, Float, Integer, Float_Formatter, Float_Formatter, Integer_Formatter);
   function Format_FFF is new Format_3 (Float, Float, Float, Float_Formatter, Float_Formatter, Float_Formatter);
   function Format_FFB is new Format_3 (Float, Float, Boolean, Float_Formatter, Float_Formatter, Formatters.Boolean_Formatter);
   
   --  All FBX combinations
   function Format_FBS (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : String) return String;
   function Format_FBI is new Format_3 (Float, Boolean, Integer, Float_Formatter, Formatters.Boolean_Formatter, Integer_Formatter);
   function Format_FBF is new Format_3 (Float, Boolean, Float, Float_Formatter, Formatters.Boolean_Formatter, Float_Formatter);
   function Format_FBB is new Format_3 (Float, Boolean, Boolean, Float_Formatter, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter);
   
   --  All BSX combinations
   function Format_BSS (Template : String; Arg1 : Boolean; Arg2, Arg3 : String) return String;
   function Format_BSI (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Integer) return String;
   function Format_BSF (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Float) return String;
   function Format_BSB (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Boolean) return String;
   
   --  All BIX combinations
   function Format_BIS (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : String) return String;
   function Format_BII is new Format_3 (Boolean, Integer, Integer, Formatters.Boolean_Formatter, Integer_Formatter, Integer_Formatter);
   function Format_BIF is new Format_3 (Boolean, Integer, Float, Formatters.Boolean_Formatter, Integer_Formatter, Float_Formatter);
   function Format_BIB is new Format_3 (Boolean, Integer, Boolean, Formatters.Boolean_Formatter, Integer_Formatter, Formatters.Boolean_Formatter);
   
   --  All BFX combinations
   function Format_BFS (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : String) return String;
   function Format_BFI is new Format_3 (Boolean, Float, Integer, Formatters.Boolean_Formatter, Float_Formatter, Integer_Formatter);
   function Format_BFF is new Format_3 (Boolean, Float, Float, Formatters.Boolean_Formatter, Float_Formatter, Float_Formatter);
   function Format_BFB is new Format_3 (Boolean, Float, Boolean, Formatters.Boolean_Formatter, Float_Formatter, Formatters.Boolean_Formatter);
   
   --  All BBX combinations
   function Format_BBS (Template : String; Arg1, Arg2 : Boolean; Arg3 : String) return String;
   function Format_BBI is new Format_3 (Boolean, Boolean, Integer, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter, Integer_Formatter);
   function Format_BBF is new Format_3 (Boolean, Boolean, Float, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter, Float_Formatter);
   function Format_BBB is new Format_3 (Boolean, Boolean, Boolean, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter, Formatters.Boolean_Formatter);

   --  UNIFIED INTERFACE: Complete overloaded Format functions
   --  Single argument overloads (4 combinations)
   function Format (Template : String; Arg : String) return String renames Format_S;
   function Format (Template : String; Arg : Integer) return String renames Format_I;
   function Format (Template : String; Arg : Float) return String renames Format_F;
   function Format (Template : String; Arg : Boolean) return String renames Format_B;

   --  Two argument overloads (16 combinations)
   function Format (Template : String; Arg1, Arg2 : String) return String renames Format_SS;
   function Format (Template : String; Arg1 : String; Arg2 : Integer) return String renames Format_SI;
   function Format (Template : String; Arg1 : String; Arg2 : Float) return String renames Format_SF;
   function Format (Template : String; Arg1 : String; Arg2 : Boolean) return String renames Format_SB;
   function Format (Template : String; Arg1 : Integer; Arg2 : String) return String renames Format_IS;
   function Format (Template : String; Arg1, Arg2 : Integer) return String renames Format_II;
   function Format (Template : String; Arg1 : Integer; Arg2 : Float) return String renames Format_IF;
   function Format (Template : String; Arg1 : Integer; Arg2 : Boolean) return String renames Format_IB;
   function Format (Template : String; Arg1 : Float; Arg2 : String) return String renames Format_FS;
   function Format (Template : String; Arg1 : Float; Arg2 : Integer) return String renames Format_FI;
   function Format (Template : String; Arg1, Arg2 : Float) return String renames Format_FF;
   function Format (Template : String; Arg1 : Float; Arg2 : Boolean) return String renames Format_FB;
   function Format (Template : String; Arg1 : Boolean; Arg2 : String) return String renames Format_BS;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Integer) return String renames Format_BI;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Float) return String renames Format_BF;
   function Format (Template : String; Arg1, Arg2 : Boolean) return String renames Format_BB;

   --  Three argument overloads (64 combinations)
   function Format (Template : String; Arg1, Arg2, Arg3 : String) return String renames Format_SSS;
   function Format (Template : String; Arg1, Arg2 : String; Arg3 : Integer) return String renames Format_SSI;
   function Format (Template : String; Arg1, Arg2 : String; Arg3 : Float) return String renames Format_SSF;
   function Format (Template : String; Arg1, Arg2 : String; Arg3 : Boolean) return String renames Format_SSB;
   function Format (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : String) return String renames Format_SIS;
   function Format (Template : String; Arg1 : String; Arg2, Arg3 : Integer) return String renames Format_SII;
   function Format (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Float) return String renames Format_SIF;
   function Format (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Boolean) return String renames Format_SIB;
   function Format (Template : String; Arg1 : String; Arg2 : Float; Arg3 : String) return String renames Format_SFS;
   function Format (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Integer) return String renames Format_SFI;
   function Format (Template : String; Arg1 : String; Arg2, Arg3 : Float) return String renames Format_SFF;
   function Format (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Boolean) return String renames Format_SFB;
   function Format (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : String) return String renames Format_SBS;
   function Format (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Integer) return String renames Format_SBI;
   function Format (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Float) return String renames Format_SBF;
   function Format (Template : String; Arg1 : String; Arg2, Arg3 : Boolean) return String renames Format_SBB;
   function Format (Template : String; Arg1 : Integer; Arg2, Arg3 : String) return String renames Format_ISS;
   function Format (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Integer) return String renames Format_ISI;
   function Format (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Float) return String renames Format_ISF;
   function Format (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Boolean) return String renames Format_ISB;
   function Format (Template : String; Arg1, Arg2 : Integer; Arg3 : String) return String renames Format_IIS;
   function Format (Template : String; Arg1, Arg2, Arg3 : Integer) return String renames Format_III;
   function Format (Template : String; Arg1, Arg2 : Integer; Arg3 : Float) return String renames Format_IIF;
   function Format (Template : String; Arg1, Arg2 : Integer; Arg3 : Boolean) return String renames Format_IIB;
   function Format (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : String) return String renames Format_IFS;
   function Format (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Integer) return String renames Format_IFI;
   function Format (Template : String; Arg1 : Integer; Arg2, Arg3 : Float) return String renames Format_IFF;
   function Format (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Boolean) return String renames Format_IFB;
   function Format (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : String) return String renames Format_IBS;
   function Format (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Integer) return String renames Format_IBI;
   function Format (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Float) return String renames Format_IBF;
   function Format (Template : String; Arg1 : Integer; Arg2, Arg3 : Boolean) return String renames Format_IBB;
   function Format (Template : String; Arg1 : Float; Arg2, Arg3 : String) return String renames Format_FSS;
   function Format (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Integer) return String renames Format_FSI;
   function Format (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Float) return String renames Format_FSF;
   function Format (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Boolean) return String renames Format_FSB;
   function Format (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : String) return String renames Format_FIS;
   function Format (Template : String; Arg1 : Float; Arg2, Arg3 : Integer) return String renames Format_FII;
   function Format (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Float) return String renames Format_FIF;
   function Format (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Boolean) return String renames Format_FIB;
   function Format (Template : String; Arg1, Arg2 : Float; Arg3 : String) return String renames Format_FFS;
   function Format (Template : String; Arg1, Arg2 : Float; Arg3 : Integer) return String renames Format_FFI;
   function Format (Template : String; Arg1, Arg2, Arg3 : Float) return String renames Format_FFF;
   function Format (Template : String; Arg1, Arg2 : Float; Arg3 : Boolean) return String renames Format_FFB;
   function Format (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : String) return String renames Format_FBS;
   function Format (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : Integer) return String renames Format_FBI;
   function Format (Template : String; Arg1 : Float; Arg2, Arg3 : Boolean) return String renames Format_FBB;
   function Format (Template : String; Arg1 : Boolean; Arg2, Arg3 : String) return String renames Format_BSS;
   function Format (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Integer) return String renames Format_BSI;
   function Format (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Float) return String renames Format_BSF;
   function Format (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Boolean) return String renames Format_BSB;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : String) return String renames Format_BIS;
   function Format (Template : String; Arg1 : Boolean; Arg2, Arg3 : Integer) return String renames Format_BII;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Float) return String renames Format_BIF;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Boolean) return String renames Format_BIB;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : String) return String renames Format_BFS;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Integer) return String renames Format_BFI;
   function Format (Template : String; Arg1 : Boolean; Arg2, Arg3 : Float) return String renames Format_BFF;
   function Format (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Boolean) return String renames Format_BFB;
   function Format (Template : String; Arg1, Arg2 : Boolean; Arg3 : String) return String renames Format_BBS;
   function Format (Template : String; Arg1, Arg2 : Boolean; Arg3 : Integer) return String renames Format_BBI;
   function Format (Template : String; Arg1, Arg2 : Boolean; Arg3 : Float) return String renames Format_BBF;
   function Format (Template : String; Arg1, Arg2, Arg3 : Boolean) return String renames Format_BBB;

end Format_Strings.Common;
