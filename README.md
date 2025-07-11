# Format_Strings

A type-safe string formatting library for Ada that provides Python/Rust-style format strings with compile-time safety and an ergonomic API.

## Features

- **Type Safety**: Generic functions ensure type correctness at compile time
- **Flexible Formatting**: Support for alignment, padding, width, precision, and base conversion
- **Extensible**: Format any type by providing a formatter function
- **Zero Runtime Overhead**: No dynamic dispatch or runtime type checking
- **Multiple Arguments**: Support for up to 3 arguments with mixed types via function overloading
- **Unified Interface**: Single `Format` function name for all type combinations

## Quick Start

### Unified Format Interface

The library provides a single overloaded `Format` function that works with any combination of String, Integer, Float, and Boolean arguments:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Format_Strings.Common; use Format_Strings.Common;

procedure Example is
begin
   -- Single arguments
   Put_Line (Format ("Number: {}", 42));
   Put_Line (Format ("Pi: {:.2f}", 3.14159));
   Put_Line (Format ("Hello, {}!", "World"));
   Put_Line (Format ("Name: {:>10}", "Alice"));  -- Right-aligned string
   Put_Line (Format ("Active: {}", True));

   -- Two arguments - any combination of types
   Put_Line (Format ("{} = {}", "The answer", 42));
   Put_Line (Format ("User ID: {}, Name: {}", 123, "Alice"));
   Put_Line (Format ("Count: {}, Average: {:.2f}", 10, 3.14159));
   Put_Line (Format ("Score: {}, Passed: {}", 85, True));

   -- Three arguments - any combination of types
   Put_Line (Format ("{} scored {} out of {}", "Bob", 85, 100));
   Put_Line (Format ("User {} from {} has {} messages", "Dave", "NYC", 12));
   Put_Line (Format ("Active: {}, Score: {:.1f}, Name: {}", True, 95.5, "Carol"));
end Example;
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
Format ("{:10}", 42)           -- "        42" (right-aligned, width 10)
Format ("{:<10}", 42)          -- "42        " (left-aligned)
Format ("{:^10}", 42)          -- "    42    " (centered)
Format ("{:0>5}", 42)          -- "00042" (zero-padded)
Format ("{:#x}", 255)          -- "0xff" (hex with prefix)
Format ("{:.2f}", 3.14159)     -- "3.14" (2 decimal places)
Format ("{:+.2e}", 1234.5)     -- "+1.23e+3" (scientific with sign)
```

### Escape Sequences

Use backslash to escape literal braces:

```ada
Format ("\{name\} = {}", "value")    -- "{name} = value"
Format ("Use \{\} for holes", 42)    -- "Use {} for holes"
```

## Extending with Custom Types

### Creating a Custom Formatter

To format your own types, create a formatter function and instantiate the generic Format function:

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

### Adding Support for More Arguments

The unified Format interface supports up to 3 arguments. For more arguments, instantiate the generic Format functions. Note that these require definite subtypes (e.g., Integer, Float, Boolean) rather than indefinite ones (e.g., String):

```ada
with Format_Strings; use Format_Strings;
with Format_Strings.Formatters;

-- For 4 arguments with definite types
declare
   function Int_Fmt is new Formatters.Integer_Formatter (Integer);
   function Float_Fmt is new Formatters.Float_Formatter (Float);

   function My_Format_4 is new Format_4
     (T1 => Integer,
      T2 => Integer,
      T3 => Float,
      T4 => Boolean,
      Formatter_1 => Int_Fmt,
      Formatter_2 => Int_Fmt,
      Formatter_3 => Float_Fmt,
      Formatter_4 => Formatters.Boolean_Formatter);
begin
   Put_Line (My_Format_4 ("Values: {}, {}, {:.1f}, {}", 1, 2, 3.14, True));
end;

-- For custom types with multiple arguments
type My_Type is ...;
function My_Formatter (Item : My_Type; Spec : Format_Spec) return String is ...;

function Format_Custom is new Format_3
  (T1 => My_Type,
   T2 => Integer,
   T3 => Boolean,
   Formatter_1 => My_Formatter,
   Formatter_2 => Int_Fmt,
   Formatter_3 => Formatters.Boolean_Formatter);
```

### Using the Generic Interface Directly

For maximum flexibility, use the base generic Format function:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Format_Strings; use Format_Strings;
with Format_Strings.Formatters;

procedure Example is
   -- Instantiate formatter for a specific type
   function Int_Fmt is new Formatters.Integer_Formatter (Integer);
   function My_Format is new Format (Integer, Int_Fmt);
begin
   Put_Line (My_Format ("Value: {:08x}", 255));  -- "Value: 000000ff"
end Example;
```

## Package Structure

- **Format_Strings**: Core generic formatting functions and types
- **Format_Strings.Common**: Pre-instantiated Format functions for common type combinations
- **Format_Strings.Formatters**: Ready-to-use formatter functions for built-in types

## Installation

Add to your Alire project:

```bash
alr with format_strings
```

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.
