# Format_Strings

A type-safe string formatting library for Ada that provides Python/Rust-style format strings with compile-time safety and an ergonomic API.

## Features

- **Type Safety**: Generic functions ensure type correctness at compile time
- **Flexible Formatting**: Support for alignment, padding, width, precision, and base conversion
- **Extensible**: Format any type by providing a formatter function
- **Zero Runtime Overhead**: No dynamic dispatch or runtime type checking
- **Multiple Arguments**: Support for up to 7 arguments with mixed types
- **Pre-instantiated Common Types**: Ready-to-use formatters for common type combinations

## Quick Start

### Simple Usage with Backward Compatibility Functions

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Format_Strings; use Format_Strings;

procedure Example is
begin
   -- Simple single-argument formatting
   Put_Line (Format_Int ("Number: {}", 42));
   Put_Line (Format_Float ("Pi: {:.2f}", 3.14159));
   Put_Line (Format_Str ("Hello, {}!", "World"));
   Put_Line (Format_Str ("Name: {:>10}", "Alice"));  -- Right-aligned string
end Example;
```

### Using Pre-instantiated Functions

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Format_Strings.Common; use Format_Strings.Common;

procedure Example is
begin
   -- Mixed types with pre-instantiated functions
   Put_Line (Format_SI ("{} = {}", "The answer", 42));
   Put_Line (Format_IS ("User ID: {}, Name: {}", 123, "Alice"));
   Put_Line (Format_IF ("Count: {}, Average: {:.2f}", 10, 3.14159));
   
   -- More arguments
   Put_Line (Format_SII ("{} scored {} out of {}", "Bob", 85, 100));
   
   -- Five arguments with mixed types
   Put_Line (Format_SISIB ("User {} (ID: {}) from {} has {} messages. Active: {}",
                           "Dave", 456, "NYC", 12, True));
end Example;
```

### Generic Interface for Maximum Flexibility

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Format_Strings; use Format_Strings;
with Format_Strings.Formatters;

procedure Example is
   -- Instantiate formatters for your types
   function Int_Fmt is new Formatters.Integer_Formatter (Integer);
   function My_Format is new Format (Integer, Int_Fmt);
begin
   Put_Line (My_Format ("Value: {:08x}", 255));  -- "Value: 000000ff"
end Example;
```

### Custom Type Formatting

```ada
with Format_Strings; use Format_Strings;
with Format_Strings.Formatters;

procedure Custom_Example is
   type Point is record
      X, Y : Float;
   end record;
   
   function Point_Formatter (P : Point; Spec : Format_Spec) return String is
   begin
      -- Format based on type specifier
      if Spec.Type_Char = 'd' then
         -- Debug format
         return "Point(X => " & P.X'Image & ", Y => " & P.Y'Image & ")";
      else
         -- Default format
         return "(" & P.X'Image & ", " & P.Y'Image & ")";
      end if;
   end Point_Formatter;
   
   function Format_Point is new Format (Point, Point_Formatter);
   
   P : constant Point := (X => 1.5, Y => 2.5);
begin
   Put_Line (Format_Point ("Position: {}", P));        -- "Position: ( 1.50000E+00,  2.50000E+00)"
   Put_Line (Format_Point ("Debug: {:d}", P));         -- "Debug: Point(X =>  1.50000E+00, Y =>  2.50000E+00)"
end Custom_Example;
```

## Format Specification

Format strings use `{}` as placeholders with optional format specifications:

```
{[position]:[fill][align][sign][#][0][width][.precision][type]}
```

- **position**: Argument position (1-based), e.g., `{2}` for second argument
- **fill**: Fill character for padding (default: space)
- **align**: `<` (left), `>` (right), `^` (center)
- **sign**: `+` to always show sign
- **#**: Alternate form (adds 0x/0b prefix for hex/binary)
- **0**: Zero-padding for numbers
- **width**: Minimum field width
- **precision**: For floats, number of decimal places
- **type**: Format type:
  - Integers: `b` (binary), `o` (octal), `x` (hex lowercase), `X` (hex uppercase)
  - Floats: `f` (fixed), `e` (scientific), `E` (scientific uppercase), `%` (percentage)

### Examples

```ada
Format_Int ("{:10}", 42)           -- "        42" (right-aligned, width 10)
Format_Int ("{:<10}", 42)          -- "42        " (left-aligned)
Format_Int ("{:^10}", 42)          -- "    42    " (centered)
Format_Int ("{:0>5}", 42)          -- "00042" (zero-padded)
Format_Int ("{:#x}", 255)          -- "0xff" (hex with prefix)
Format_Float ("{:.2f}", 3.14159)   -- "3.14" (2 decimal places)
Format_Float ("{:+.2e}", 1234.5)   -- "+1.23e+3" (scientific with sign)
```

### Escape Sequences

Use backslash to escape literal braces:

```ada
Format_Str ("\{name\} = {}", "value")    -- "{name} = value"
Format_Int ("Use \{\} for holes", 42)    -- "Use {} for holes"
```

## Pre-instantiated Functions

The `Format_Strings.Common` package provides ready-to-use format functions:

### Single Argument
- `Format_I` - Integer
- `Format_F` - Float  
- `Format_B` - Boolean

### Two Arguments
- `Format_SI` - String, Integer
- `Format_IS` - Integer, String
- `Format_II` - Integer, Integer
- `Format_IF` - Integer, Float
- `Format_FI` - Float, Integer
- `Format_SS` - String, String

### Three Arguments
- `Format_SII` - String, Integer, Integer
- `Format_ISI` - Integer, String, Integer
- `Format_SSI` - String, String, Integer
- `Format_IIF` - Integer, Integer, Float

### Four+ Arguments
- `Format_SSII` - String, String, Integer, Integer
- `Format_ISIF` - Integer, String, Integer, Float
- `Format_SISIB` - String, Integer, String, Integer, Boolean

## Creating Your Own Instantiations

For type combinations not covered by the pre-instantiated functions:

```ada
with Format_Strings; use Format_Strings;
with Format_Strings.Formatters;

-- For a single custom type
type My_Type is ...;
function My_Formatter (Item : My_Type; Spec : Format_Spec) return String is ...;
function Format_My is new Format (My_Type, My_Formatter);

-- For multiple arguments with mixed types
function Format_MISI is new Format_4 
  (My_Type, Integer, String, Integer,
   My_Formatter, Formatters.Integer_Formatter, 
   Formatters.String_Formatter, Formatters.Integer_Formatter);
```

## Installation

Add to your Alire project:

```bash
alr with format_strings
```

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.
