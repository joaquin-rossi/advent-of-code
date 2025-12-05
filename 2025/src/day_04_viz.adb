with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada;
use Interfaces;

procedure Day_04_Viz is
    package Cell_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Boolean);
    use Cell_Vec;

    package Grid_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Cell_Vec.Vector);
    use Cell_Vec;

    Width  : Integer := 0;
    Height : Integer := 0;
    Grid   : Grid_Vec.Vector := Grid_Vec.Empty_Vector;

    function Get (X : Integer; Y : Integer) return Boolean is
    begin
        if not (X in 1 .. Width and Y in 1 .. Height) then
            return False;
        end if;

        return Grid (Y) (X);
    end;

    procedure Put_PPM (Width : Positive; Height : Positive) is
    begin
        Put_Line ("P6");

        Integer_Text_IO.Put (Width, Width => 0);
        Put (" ");
        Integer_Text_IO.Put (Height, Width => 0);
        New_Line;

        Put_Line ("255");

    end;

    type Color is record
        R : Unsigned_8;
        G : Unsigned_8;
        B : Unsigned_8;
    end record;

    procedure Put (c : Color) is
    begin
        Put (Character'Val (c.R));
        Put (Character'Val (c.G));
        Put (Character'Val (c.B));
    end;

    procedure Run is
        X     : Integer;
        Y     : Integer;
        Xp    : Integer;
        Yp    : Integer;
        Neigh : Integer;

        Grid_New     : Grid_Vec.Vector;
        Grid_Changed : Boolean := True;

        Color_Empty   : Color := (R => 0, G => 0, B => 0);
        Color_Filled  : Color := (R => 255, G => 255, B => 255);
        Color_Changed : Color := (R => 255, G => 0, B => 0);
    begin
        begin
            Put_PPM (Width, Height);
            for Y in 1 .. Height loop
                for X in 1 .. Width loop
                    if Get (X, Y) then
                        Put (Color_Filled);
                    else
                        Put (Color_Empty);
                    end if;
                end loop;
            end loop;
        end;


        while Grid_Changed loop
            Grid_New := Grid;
            Grid_Changed := False;

            Put_PPM (Width, Height);

            for Y in 1 .. Height loop
                for X in 1 .. Width loop
                    if Get (X, Y) then
                        Neigh := 0;

                        for Yp in -1 .. 1 loop
                            for Xp in -1 .. 1 loop
                                if not (Xp = 0 and Yp = 0)
                                   and Get (X + Xp, Y + Yp)
                                then
                                    Neigh := Neigh + 1;
                                end if;
                            end loop;
                        end loop;

                        if Neigh < 4 then
                            Grid_Changed := True;
                            Grid_New (Y) (X) := False;
                            Put (Color_Changed);
                        else
                            Put (Color_Filled);
                        end if;
                    else
                        Put (Color_Empty);
                    end if;
                end loop;
            end loop;

            Grid := Grid_New;
        end loop;
    end;

    Line : Unbounded_String;
begin
    while not End_Of_File loop
        Line := To_Unbounded_String (Get_Line);
        if Width = 0 then
            Width := Length (Line);
        end if;
        Height := Height + 1;

        declare
            I   : Integer;
            Row : Cell_Vec.Vector := Cell_Vec.Empty_Vector;
        begin
            for I in 1 .. Width loop
                Cell_Vec.Append
                   (Row,
                    (case Element (Line, I) is
                        when '@'    => True,
                        when '.'    => False,
                        when others => raise Constraint_Error));
            end loop;

            Grid_Vec.Append (Grid, Row);
        end;
    end loop;

    Run;
end;
