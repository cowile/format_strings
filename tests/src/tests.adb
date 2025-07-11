with Ada.Assertions;
with Ada.Text_IO;
with Format_Strings;
with Format_Strings.Common;

use Ada.Assertions;
use Ada.Text_IO;
use Format_Strings.Common;

procedure Tests is

   procedure Test_Basic_Integer is
   begin
      Put_Line ("Testing basic integer formatting...");
      Assert (Format ("Number: {}", 42) = "Number: 42");
      Assert (Format ("Negative: {}", -42) = "Negative: -42");
      Put_Line ("  Basic integer: PASSED");
   end Test_Basic_Integer;

   procedure Test_Multiple_Args is
   begin
      Put_Line ("Testing multiple arguments...");
      Assert (Format ("x = {}, y = {}", 10, 20) = "x = 10, y = 20");
      Put_Line ("  Multiple args: PASSED");
   end Test_Multiple_Args;

   procedure Test_Positional_Args is
      Result : constant String := Format ("{2} before {1}", 10, 20);
   begin
      Put_Line ("Testing positional arguments...");
      Put_Line ("  Expected: '20 before 10'");
      Put_Line ("  Got:      '" & Result & "'");
      Assert (Result = "20 before 10");
      Put_Line ("  Positional args: PASSED");
   end Test_Positional_Args;

   procedure Test_Width_Formatting is
   begin
      Put_Line ("Testing width formatting...");
      Assert (Format (">{:5}<", 42) = ">   42<");
      Assert (Format (">{:>5}<", 42) = ">   42<");  -- Right is default
      Assert (Format (">{:<5}<", 42) = ">42   <");
      Assert (Format (">{:^5}<", 42) = "> 42  <");
      Put_Line ("  Width formatting: PASSED");
   end Test_Width_Formatting;

   procedure Test_Number_Bases is
   begin
      Put_Line ("Testing number bases...");
      Assert (Format ("Hex: {:#x}", 255) = "Hex: 0xff");
      Assert (Format ("HEX: {:#X}", 255) = "HEX: 0XFF");
      Assert (Format ("Oct: {:#o}", 64) = "Oct: 0100");
      Assert (Format ("Bin: {:#b}", 42) = "Bin: 0b101010");
      Put_Line ("  Number bases: PASSED");
   end Test_Number_Bases;

   procedure Test_Sign_Formatting is
   begin
      Put_Line ("Testing sign formatting...");
      Assert (Format ("{:+}", 42) = "+42");
      Assert (Format ("{:+}", -42) = "-42");
      Put_Line ("  Sign formatting: PASSED");
   end Test_Sign_Formatting;

   procedure Test_String_Formatting is
   begin
      Put_Line ("Testing string formatting...");
      Assert (Format ("Hello, {}!", "World") = "Hello, World!");
      Assert (Format ("{:.3}", "Hello") = "Hel");
      Put_Line ("  String formatting: PASSED");
   end Test_String_Formatting;

   procedure Test_Hole_Counting is
   begin
      Put_Line ("Testing hole counting...");
      Assert (Format_Strings.Count_Holes ("") = 0);
      Assert (Format_Strings.Count_Holes ("No holes") = 0);
      Assert (Format_Strings.Count_Holes ("One {} hole") = 1);
      Assert (Format_Strings.Count_Holes ("{} and {}") = 2);
      Assert (Format_Strings.Count_Holes ("\{escaped\}") = 0);  --  Escaped
      Assert (Format_Strings.Count_Holes ("Mix {} and \{escaped\}") = 1);
      Put_Line ("  Hole counting: PASSED");
   end Test_Hole_Counting;

   procedure Test_Zero_Padding is
   begin
      Put_Line ("Testing zero padding...");

      --  Basic zero padding
      Assert (Format ("{:05}", 42) = "00042");
      Assert (Format ("{:05}", -42) = "-0042");

      --  Zero padding with different bases
      Assert (Format ("{:#010x}", 255) = "0x000000ff");
      Assert (Format ("{:#010X}", 255) = "0X000000FF");
      Assert (Format ("{:#08b}", 15) = "0b001111");

      --  Zero padding with sign
      Assert (Format ("{:+05}", 42) = "+0042");
      Assert (Format ("{:+05}", -42) = "-0042");

      --  Zero padding is ignored with explicit alignment
      Assert (Format ("{:<05}", 42) = "42   ");
      Assert (Format ("{:>05}", 42) = "   42");

      Put_Line ("  Zero padding: PASSED");
   end Test_Zero_Padding;

   procedure Test_Float_Formatting is
   begin
      Put_Line ("Testing float formatting...");

      --  Basic float formatting
      Assert (Format ("Pi: {}", 3.14159) = "Pi: 3.141590");

      --  Precision control
      Assert (Format ("Pi: {:.2f}", 3.14159) = "Pi: 3.14");
      Assert (Format ("Pi: {:.4f}", 3.14159) = "Pi: 3.1415");
      Assert (Format ("Int: {:.0f}", 3.14159) = "Int: 3");

      --  Sign control
      Assert (Format ("{:+.2f}", 3.14) = "+3.14");
      Assert (Format ("{:+.2f}", -3.14) = "-3.14");

      --  Scientific notation
      Assert (Format ("{:.2e}", 1234.5) = "1.23e+3");
      Assert (Format ("{:.2E}", 1234.5) = "1.23E+3");
      Assert (Format ("{:.3e}", 0.00123) = "1.230e-3");

      --  Percentage
      Assert (Format ("{:.1%}", 0.125) = "12.5%");
      Assert (Format ("{:.0%}", 0.99) = "99%");

      --  Width and alignment
      Assert (Format ("{:10.2f}", 3.14) = "      3.14");
      Assert (Format ("{:<10.2f}", 3.14) = "3.14      ");
      Assert (Format ("{:^10.2f}", 3.14) = "   3.14   ");

      Put_Line ("  Float formatting: PASSED");
   end Test_Float_Formatting;

   procedure Test_Escape_Sequences is
   begin
      Put_Line ("Testing escape sequences...");

      --  Escaped braces
      Assert (Format ("\{not a hole\}", 42) = "{not a hole}");
      Assert (Format ("Use \{\} for holes", "test") = "Use {} for holes");

      --  Mixed escaped and real holes
      Assert
        (Format ("Real {} and \{escaped\}", 42) = "Real 42 and {escaped}");
      Assert (Format ("\{} {} \{} {}", 1, 2) = "{} 1 {} 2");

      --  Escaped backslash
      Assert
        (Format ("Path: C:\\Users\\{}", "Alice") = "Path: C:\Users\Alice");

      --  Escaped characters in format specs
      Assert (Format ("Value: \{{}}", "test") = "Value: {test}");

      Put_Line ("  Escape sequences: PASSED");
   end Test_Escape_Sequences;

   procedure Test_Unified_Interface is
   begin
      Put_Line ("Testing unified Format interface (PRIMARY INTERFACE)...");

      --  Single argument overloads - the new way to format!
      Assert (Format ("String: {}", "Hello") = "String: Hello");
      Assert (Format ("Integer: {}", 42) = "Integer: 42");
      Assert (Format ("Float: {:.2f}", 3.14159) = "Float: 3.14");
      Assert (Format ("Boolean: {}", True) = "Boolean: True");

      --  Two argument overloads - mixed types work seamlessly
      Assert (Format ("Two strings: {} {}", "Hello", "World") =
              "Two strings: Hello World");
      Assert (Format ("String + Integer: {} = {}", "Answer", 42) =
              "String + Integer: Answer = 42");
      Assert (Format ("Two integers: {} + {}", 2, 3) = "Two integers: 2 + 3");
      Assert (Format ("Float + Integer: {} + {}", 3.14, 42) =
              "Float + Integer: 3.140000 + 42");
      Assert (Format ("Integer + Boolean: {} is {}", 1, True) =
              "Integer + Boolean: 1 is True");

      --  Three argument overloads - powerful combinations
      Assert (Format ("Three strings: {} {} {}", "One", "Two", "Three") =
              "Three strings: One Two Three");
      Assert (Format ("String Int String: {} {} {}", "Value", 42, "End") =
              "String Int String: Value 42 End");
      Assert (Format ("All integers: {} {} {}", 1, 2, 3) =
              "All integers: 1 2 3");
      Assert (Format ("All floats: {} {} {}", 1.1, 2.2, 3.3) =
              "All floats: 1.100000 2.200000 3.299999");

      --  Format specifiers work with unified interface
      Assert (Format ("Hex: {:#x}", 255) = "Hex: 0xff");
      Assert (Format ("Width: {:>5}", 42) = "Width:    42");
      Assert (Format ("Precision: {:.2f}", 3.14159) = "Precision: 3.14");

      --  Positional arguments work with 1-based indexing
      Assert (Format ("{2} before {1}", "cart", "horse") =
              "horse before cart");
      Assert (Format ("{3} {1} {2}", "B", "C", "A") = "A B C");

      Put_Line ("  + Unified Format interface: PASSED");
      Put_Line ("  + This is the ONLY interface for Format_Strings!");
   end Test_Unified_Interface;

begin
   Put_Line ("Running Format_Strings v1.0 Tests (Unified Interface Only)");
   Put_Line ("===========================================================");

   --  CORE FUNCTIONALITY TESTS
   Test_Basic_Integer;
   Test_Multiple_Args;
   Test_Positional_Args;
   Test_Width_Formatting;
   Test_Number_Bases;
   Test_Sign_Formatting;
   Test_String_Formatting;
   Test_Hole_Counting;
   Test_Zero_Padding;
   Test_Float_Formatting;
   Test_Escape_Sequences;
   New_Line;

   --  PRIMARY INTERFACE TESTS (main interface)
   Test_Unified_Interface;

   New_Line;
   Put_Line ("All tests PASSED!");
   Put_Line ("Format_Strings unified interface is clean and simple!");
end Tests;
