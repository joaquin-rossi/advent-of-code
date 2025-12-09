with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_09 is
    type Point is record
        X : Big_Integer;
        Y : Big_Integer;
    end record;

    package P_Vecs is new
       Vectors (Index_Type => Positive, Element_Type => Point);
    use P_Vecs;

    Points : P_Vecs.Vector;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;

    Area : Big_Integer;
    PI   : Point;
    PJ   : Point;
begin
    while not End_Of_File loop
        declare
            Line    : Unbounded_String := To_Unbounded_String (Get_Line);
            Comma_I : Integer := Index (Line, ",", 1);
            P       : Point;
        begin
            P.X := From_String (Slice (Line, 1, Comma_I - 1));
            P.Y := From_String (Slice (Line, Comma_I + 1, Length (Line)));
            Append (Points, P);
        end;
    end loop;

    for I in First_Index (Points) .. Last_Index (Points) loop
        PI := Points (I);
        for J in I + 1 .. Last_Index (Points) loop
            PJ := Points (J);
            Area := (abs (PI.X - PJ.X) + 1) * (abs (PI.Y - PJ.Y) + 1);

            if Area > Silver then
                Silver := Area;
            end if;
        end loop;
    end loop;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
