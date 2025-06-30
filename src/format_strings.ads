with Ada.Strings;
with Ada.Strings.Bounded;

package Format_Strings is
   package Bounded_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (256);

   Escape_Character : Character := '\';
   Start_Hole_Character : Character := '{';
   End_Hole_Character : Character := '}';

   subtype Template is Bounded_Strings.Bounded_String;
   type Pieces is
      array (Natural range <>) of Bounded_Strings.Bounded_String;

   type Format_String (Parts : Natural) is
      record
         Given : Template;
         Holes : Natural := Parts / 2;
         Filled : Natural := 0;
         Constructed : Pieces (0 .. Parts);
      end record;

   function Count_Holes (T : Template) return Natural;
   function Create (T : Template) return Format_String;
   function Create (S : String) return Format_String;

   generic
      type T is private;
   procedure Format (FS : in out Format_String; Arg : T);

   function To_String (FS : Format_String) return String;

end Format_Strings;
