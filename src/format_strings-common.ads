--  Format_Strings.Common: Modern Unified Formatting Interface
--
--  This package provides the RECOMMENDED way to use Format_Strings through
--  a single overloaded "Format" function that works with all type combinations.
--
--  QUICK START:
--    with Format_Strings.Common; use Format_Strings.Common;
--
--    Result := Format("Hello {}", "World");            -- String formatting
--    Result := Format("Number: {}", 42);               -- Integer formatting
--    Result := Format("Pi: {:.2f}", 3.14159);          -- Float with precision
--    Result := Format("Values: {} {}", "Text", 123);   -- Mixed types
--    Result := Format("Order: {1} {0}", "Second", "First"); -- Positional
--
--  The unified Format function supports:
--  â¢ All combinations of String, Integer, Float, Boolean (up to 3 arguments)
--  â¢ Full format specifier syntax: alignment, width, precision, type
--  â¢ Positional arguments and automatic sequential assignment
--  â¢ Type-safe compile-time resolution
--  â¢ Zero runtime overhead through Ada generics
--
--  This replaces the old numbered functions (Format_Int, Format_2, etc.)
--  while maintaining full backward compatibility.

with Format_Strings.Formatters;

package Format_Strings.Common is

   --  ==========================================================================
   --  I. INTERNAL FORMATTERS (Pre-instantiated for standard types)
   --  ==========================================================================

   function Integer_Formatter is new Formatters.Integer_Formatter (Integer);
   function Float_Formatter is new Formatters.Float_Formatter (Float);

   --  ==========================================================================
   --  II. UNIFIED FORMAT INTERFACE (RECOMMENDED PRIMARY INTERFACE)
   --  ==========================================================================
   --  Use these overloaded Format functions for clean, modern code:
   --    Result := Format("Hello {}", "World");
   --    Result := Format("Value: {}", 42);
   --    Result := Format("Mix: {} {}", "Text", 123);

   --  Single argument overloads (4 combinations)
   function Format (Template : String; Arg : String) return String;
   function Format (Template : String; Arg : Integer) return String;
   function Format (Template : String; Arg : Float) return String;
   function Format (Template : String; Arg : Boolean) return String;

   --  Two argument overloads (16 combinations)
   function Format (Template : String; Arg1, Arg2 : String) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Integer) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Float) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : String) return String;
   function Format (Template : String; Arg1, Arg2 : Integer) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : String) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer) return String;
   function Format (Template : String; Arg1, Arg2 : Float) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float) return String;
   function Format (Template : String; Arg1, Arg2 : Boolean) return String;

   --  Three argument overloads (64 combinations)
   function Format
     (Template : String; Arg1, Arg2, Arg3 : String) return String;
   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Float) return String;
   function Format
     (Template : String; Arg1, Arg2 : String; Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Integer; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Float) return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Float; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2 : Boolean; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : String; Arg2, Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : String) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : String; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : String) return String;
   function Format
     (Template : String; Arg1, Arg2, Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : Float) return String;
   function Format
     (Template : String; Arg1, Arg2 : Integer; Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : Float) return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Float; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2 : Boolean; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Integer; Arg2, Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : String) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : String; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Integer; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : String) return String;
   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : Integer) return String;
   function Format (Template : String; Arg1, Arg2, Arg3 : Float) return String;
   function Format
     (Template : String; Arg1, Arg2 : Float; Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2 : Boolean; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Float; Arg2, Arg3 : Boolean) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : String) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : String; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Float)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Integer; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : String)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Integer)
      return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2, Arg3 : Float) return String;
   function Format
     (Template : String; Arg1 : Boolean; Arg2 : Float; Arg3 : Boolean)
      return String;
   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : String) return String;
   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : Integer) return String;
   function Format
     (Template : String; Arg1, Arg2 : Boolean; Arg3 : Float) return String;
   function Format
     (Template : String; Arg1, Arg2, Arg3 : Boolean) return String;

end Format_Strings.Common;
