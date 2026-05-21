
' https://acmp.ru/index.asp?main=task&id_task=801
' computational geometry + convex+ minimax + nested ternary search + numerical methods


Option Strict On

Imports System
Imports System.IO
Imports System.Math
Imports System.Globalization
Imports System.Collections.Generic

Structure rec
    Public a As Double
    Public b As Double
    Public c As Double

    Public Sub New(ByVal aa As Double, ByVal bb As Double, ByVal cc As Double)
        a = aa
        b = bb
        c = cc
    End Sub
End Structure

Structure ret
    Public f As Double
    Public y As Double

    Public Sub New(ByVal ff As Double, ByVal yy As Double)
        f = ff
        y = yy
    End Sub
End Structure

Class rdr
    Private ReadOnly s As Stream
    Private ReadOnly buf(65535) As Byte
    Private ptr As Integer
    Private len As Integer

    Public Sub New(ByVal ss As Stream)
        s = ss
        ptr = 0
        len = 0
    End Sub

    Private Function rb() As Integer
        If ptr >= len Then
            len = s.Read(buf, 0, buf.Length)
            ptr = 0

            If len = 0 Then
                Return -1
            End If
        End If

        Dim v As Integer = buf(ptr)
        ptr += 1
        Return v
    End Function

    Public Function ni() As Integer
        Return CInt(nd())
    End Function

    Public Function nd() As Double
        Dim ch As Integer = rb()

        While ch <> -1 AndAlso ch <= 32
            ch = rb()
        End While

        Dim sg As Double = 1.0

        If ch = AscW("-"c) Then
            sg = -1.0
            ch = rb()
        End If

        Dim v As Double = 0.0

        While ch >= AscW("0"c) AndAlso ch <= AscW("9"c)
            v = v * 10.0 + CDbl(ch - AscW("0"c))
            ch = rb()
        End While

        If ch = AscW("."c) Then
            Dim k As Double = 0.1
            ch = rb()

            While ch >= AscW("0"c) AndAlso ch <= AscW("9"c)
                v += CDbl(ch - AscW("0"c)) * k
                k *= 0.1
                ch = rb()
            End While
        End If

        If ch = AscW("e"c) OrElse ch = AscW("E"c) Then
            ch = rb()

            Dim es As Integer = 1

            If ch = AscW("-"c) Then
                es = -1
                ch = rb()
            ElseIf ch = AscW("+"c) Then
                ch = rb()
            End If

            Dim ev As Integer = 0

            While ch >= AscW("0"c) AndAlso ch <= AscW("9"c)
                ev = ev * 10 + ch - AscW("0"c)
                ch = rb()
            End While

            v *= Pow(10.0, CDbl(es * ev))
        End If

        Return sg * v
    End Function
End Class

Module prog
    Dim mem As New List(Of rec)()

    Function eval(ByVal x As Double, ByVal y As Double) As Double
        Dim ans As Double = 0.0

        For Each r As rec In mem
            Dim v As Double = Abs(r.a * x + r.b * y + r.c)
            ans = Max(ans, v)
        Next

        Return ans
    End Function

    Function scan_y(ByVal x As Double) As ret
        Dim l As Double = -1000000000.0
        Dim r As Double = 1000000000.0

        For i As Integer = 0 To 89
            Dim m0 As Double = l + (r - l) / 3.0
            Dim m1 As Double = r - (r - l) / 3.0

            Dim f0 As Double = eval(x, m0)
            Dim f1 As Double = eval(x, m1)

            If f0 < f1 Then
                r = m1
            Else
                l = m0
            End If
        Next

        Dim y As Double = (l + r) / 2.0
        Return New ret(eval(x, y), y)
    End Function

    Sub Main()
        Dim ins As Stream = Console.OpenStandardInput()
        Dim outs As TextWriter = Console.Out

        If File.Exists("INPUT.TXT") Then
            ins = New FileStream("INPUT.TXT", FileMode.Open, FileAccess.Read)
            outs = New StreamWriter("OUTPUT.TXT")
        End If

        Dim rd As New rdr(ins)

        Dim n As Integer = rd.ni()
        mem.Capacity = n

        For i As Integer = 0 To n - 1
            Dim x0 As Double = rd.nd()
            Dim y0 As Double = rd.nd()
            Dim x1 As Double = rd.nd()
            Dim y1 As Double = rd.nd()

            Dim a As Double = y0 - y1
            Dim b As Double = x1 - x0
            Dim c As Double = x0 * y1 - x1 * y0

            Dim v As Double = Sqrt(a * a + b * b)

            a /= v
            b /= v
            c /= v

            mem.Add(New rec(a, b, c))
        Next

        Dim l As Double = -1000000000.0
        Dim r As Double = 1000000000.0

        For i As Integer = 0 To 89
            Dim m0 As Double = l + (r - l) / 3.0
            Dim m1 As Double = r - (r - l) / 3.0

            Dim f0 As Double = scan_y(m0).f
            Dim f1 As Double = scan_y(m1).f

            If f0 < f1 Then
                r = m1
            Else
                l = m0
            End If
        Next

        Dim ax As Double = (l + r) / 2.0
        Dim ay As Double = scan_y(ax).y

        outs.WriteLine(
            ax.ToString("F10", CultureInfo.InvariantCulture) &
            " " &
            ay.ToString("F10", CultureInfo.InvariantCulture)
        )

        outs.Flush()
    End Sub
End Module
