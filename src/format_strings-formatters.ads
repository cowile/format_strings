--  Generic formatters for the Format_Strings library
--  This package provides the type class pattern for formatting any type

package Format_Strings.Formatters is

   --  Generic formatter function type
   generic
      type T is private;
   function Default_Image (Item : T; Spec : Format_Spec) return String;

   --  Predefined formatters for standard types

   --  Integer types
   generic
      type Int_Type is range <>;
   function Integer_Formatter
     (Item : Int_Type; Spec : Format_Spec) return String;

   --  Modular types
   generic
      type Mod_Type is mod <>;
   function Modular_Formatter
     (Item : Mod_Type; Spec : Format_Spec) return String;

   --  Floating point types
   generic
      type Float_Type is digits <>;
   function Float_Formatter
     (Item : Float_Type; Spec : Format_Spec) return String;

   --  String formatter (non-generic)
   function String_Formatter (Item : String; Spec : Format_Spec) return String;

   --  Character formatter (non-generic)
   function Character_Formatter
     (Item : Character; Spec : Format_Spec) return String;

   --  Boolean formatter (non-generic)
   function Boolean_Formatter
     (Item : Boolean; Spec : Format_Spec) return String;

end Format_Strings.Formatters;


