with Ada.Containers.Generic_Sort;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada;

procedure Day_08 is
    type Point is record
        X : Integer;
        Y : Integer;
        Z : Integer;
    end record;

    function Square (X : Long_Float) return Long_Float is
    begin
        return X * X;
    end;

    function Dist2 (A : Point; B : Point) return Long_Float is
    begin
        return
           Square (Long_Float (A.X - B.X))
           + Square (Long_Float (A.Y - B.Y))
           + Square (Long_Float (A.Z - B.Z));
    end;

    package Int_Vecs is new
       Vectors (Index_Type => Positive, Element_Type => Integer);
    use Int_Vecs;

    package P_Vecs is new
       Vectors (Index_Type => Positive, Element_Type => Point);
    use P_Vecs;

    Points : P_Vecs.Vector;

    type Circuit is record
        Id     : Integer;
        Parent : Integer;
        Size   : Integer;
    end record;

    package Int_Circuit_Maps is new
       Ordered_Maps (Key_Type => Integer, Element_Type => Circuit);
    use Int_Circuit_Maps;

    subtype Circuit_Ref is Int_Circuit_Maps.Reference_Type;

    Circuits : Int_Circuit_Maps.Map;

    procedure Add (N : Integer) is
    begin
        Insert (Circuits, N, (Id => N, Parent => N, Size => 1));
    end;

    function Find (N : Integer) return Integer is
    begin
        declare
            C : Circuit_Ref := Reference (Circuits, N);
        begin
            if C.Parent = N then
                return N;
            else
                C.Parent := Find (C.Parent);
                return C.Parent;
            end if;
        end;
    end;

    procedure Union (A : Integer; B : Integer) is
        X : Circuit_Ref := Reference (Circuits, Find (A));
        Y : Circuit_Ref := Reference (Circuits, Find (B));
    begin
        if X.Id = Y.Id then
            return;
        end if;

        if X.Size >= Y.Size then
            Y.Parent := X.Id;
            X.Size := X.Size + Y.Size;
        else
            X.Parent := Y.Id;
            Y.Size := Y.Size + X.Size;
        end if;
    end;

    type Pair is record
        Dist2 : Long_Float;
        I, J  : Integer;
    end record;

    function "<" (L, R : Pair) return Boolean
    is (L.Dist2 < R.Dist2);

    package Pair_Sets is new Ordered_Sets (Element_Type => Pair);
    use Pair_Sets;

    Pairs   : Pair_Sets.Set;
    Pairs_C : Pair_Sets.Cursor;

    Num_Pairs_Silver : Integer;
    Silver           : Integer;
    Gold             : Big_Integer;
begin
    if Command_Line.Argument_Count /= 1 then
        raise Constraint_Error;
    end if;
    Num_Pairs_Silver := Integer'Value (Command_Line.Argument (1));

    while not End_Of_File loop
        declare
            Line    : Unbounded_String := To_Unbounded_String (Get_Line);
            P       : Point;
            Comma_1 : Positive := Index (Line, ",", 1);
            Comma_2 : Positive := Index (Line, ",", Comma_1 + 1);
        begin
            P.X := Integer'Value (Slice (Line, 1, Comma_1 - 1));
            P.Y := Integer'Value (Slice (Line, Comma_1 + 1, Comma_2 - 1));
            P.Z := Integer'Value (Slice (Line, Comma_2 + 1, Length (Line)));

            Append (Points, P);
            Add (Last_Index (Points));
        end;
    end loop;

    for I in First_Index (Points) .. Last_Index (Points) loop
        for J in I + 1 .. Last_Index (Points) loop
            declare
                P : Pair :=
                   (I => I, J => J, Dist2 => Dist2 (Points (I), Points (J)));
            begin
                Pairs.Insert (P);
            end;
        end loop;
    end loop;

    declare
        Count : Natural := 0;
        P     : Pair;
    begin
        Pairs_C := Pair_Sets.First (Pairs);
        while Has_Element (Pairs_C) and then Count < Num_Pairs_Silver loop
            P := Element (Pairs_C);
            Union (P.I, P.J);

            Count := Count + 1;
            Pairs_C := Next (Pairs_C);
        end loop;
    end;

    declare
        Sizes : Int_Vecs.Vector;

        function Before (L, R : Positive) return Boolean is
        begin
            return Element (Sizes, L) > Element (Sizes, R);
        end;

        procedure Swap (L, R : Positive) is
        begin
            Swap (Sizes, L, R);
        end;

        procedure Sort is new
           Ada.Containers.Generic_Sort
              (Index_Type => Positive,
               Before     => Before,
               Swap       => Swap);
    begin
        for I in First_Index (Points) .. Last_Index (Points) loop
            declare
                C : Circuit := Circuits (I);
            begin
                if C.Id = C.Parent then
                    Append (Sizes, C.Size);
                end if;
            end;
        end loop;

        Sort (First_Index (Sizes), Last_Index (Sizes));

        Silver := Sizes (1) * Sizes (2) * Sizes (3);
    end;

    Put_Line ("Silver:" & Silver'Image);

    declare
        P : Pair;
    begin
        while Has_Element (Pairs_C) loop
            P := Element (Pairs_C);
            Union (P.I, P.J);

            if Circuits (Find (P.I)).Size = Integer (Length (Points)) then
                exit;
            end if;

            Pairs_C := Next (Pairs_C);
        end loop;

        Gold :=
           To_Big_Integer (Points (P.I).X) * To_Big_Integer (Points (P.J).X);
    end;

    Put_Line ("Gold:" & Gold'Image);
end;
