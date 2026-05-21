
' https://acmp.ru/index.asp?main=task&id_task=801
' computational geometry + convex+ minimax + nested ternary search + numerical methods


Option Strict On

Imports System
Imports System.IO
Imports System.Math
Imports System.Globalization

Class rdr
    Private ReadOnly s As Stream
    Private ReadOnly buf(65535) As Byte
    Private ptr As Integer
    Private len As Integer

    Public Sub New(ByVal ss As Stream)
        s = ss
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
        Dim ch As Integer = rb()

        While ch <> -1 AndAlso ch <= 32
            ch = rb()
        End While

        Dim sg As Integer = 1

        If ch = 45 Then
            sg = -1
            ch = rb()
        End If

        Dim v As Integer = 0

        While ch >= 48 AndAlso ch <= 57
            v = v * 10 + ch - 48
            ch = rb()
        End While

        Return sg * v
    End Function
End Class

Module prog
    Const box As Double = 1000000000.0
    Const eps As Double = 0.000000001
    Const inf As Double = 1.0E+100

    Dim n As Integer
    Dim m As Integer

    Dim aa() As Double
    Dim bb() As Double
    Dim cc() As Double

    Dim ha() As Double
    Dim hb() As Double
    Dim hc() As Double
    Dim hk() As Double

    Dim gx As Double = 0.7548776662466927
    Dim gy As Double = 0.6558653818196902

    Function ok(ByVal rad As Double, ByRef ox As Double, ByRef oy As Double) As Boolean
        Dim x As Double
        Dim y As Double

        If gx >= 0.0 Then
            x = -box
        Else
            x = box
        End If

        If gy >= 0.0 Then
            y = -box
        Else
            y = box
        End If

        For i As Integer = 4 To m - 1
            Dim ai As Double = ha(i)
            Dim bi As Double = hb(i)
            Dim ci As Double = hc(i) + hk(i) * rad

            If ai * x + bi * y > ci + eps Then
                Dim den As Double = ai * ai + bi * bi

                Dim x0 As Double = ai * ci / den
                Dim y0 As Double = bi * ci / den

                Dim dx As Double = -bi
                Dim dy As Double = ai

                Dim l As Double = -inf
                Dim r As Double = inf

                For j As Integer = 0 To i - 1
                    Dim cj As Double = hc(j) + hk(j) * rad

                    Dim q As Double = ha(j) * dx + hb(j) * dy
                    Dim s As Double = cj - ha(j) * x0 - hb(j) * y0

                    If Abs(q) < eps Then
                        If ha(j) * x0 + hb(j) * y0 > cj + eps Then
                            Return False
                        End If
                    ElseIf q > 0.0 Then
                        Dim v As Double = s / q

                        If v < r Then
                            r = v
                        End If
                    Else
                        Dim v As Double = s / q

                        If v > l Then
                            l = v
                        End If
                    End If

                    If l > r + eps Then
                        Return False
                    End If
                Next

                Dim sl As Double = gx * dx + gy * dy
                Dim t As Double

                If sl > 0.0 Then
                    t = l
                ElseIf sl < 0.0 Then
                    t = r
                Else
                    t = 0.0

                    If t < l Then
                        t = l
                    End If

                    If t > r Then
                        t = r
                    End If
                End If

                x = x0 + dx * t
                y = y0 + dy * t
            End If
        Next

        ox = x
        oy = y

        Return True
    End Function

    Sub swp(ByVal i As Integer, ByVal j As Integer)
        Dim ta As Double = ha(i)
        Dim tb As Double = hb(i)
        Dim tc As Double = hc(i)
        Dim tk As Double = hk(i)

        ha(i) = ha(j)
        hb(i) = hb(j)
        hc(i) = hc(j)
        hk(i) = hk(j)

        ha(j) = ta
        hb(j) = tb
        hc(j) = tc
        hk(j) = tk
    End Sub

    Sub Main()
        Dim ins As Stream = Console.OpenStandardInput()
        Dim outs As TextWriter = Console.Out

        If File.Exists("INPUT.TXT") Then
            ins = New FileStream("INPUT.TXT", FileMode.Open, FileAccess.Read)
            outs = New StreamWriter("OUTPUT.TXT")
        End If

        Dim rd As New rdr(ins)

        n = rd.ni()

        ReDim aa(n - 1)
        ReDim bb(n - 1)
        ReDim cc(n - 1)

        Dim hi As Double = 0.0

        For i As Integer = 0 To n - 1
            Dim x0 As Double = CDbl(rd.ni())
            Dim y0 As Double = CDbl(rd.ni())
            Dim x1 As Double = CDbl(rd.ni())
            Dim y1 As Double = CDbl(rd.ni())

            Dim a As Double = y0 - y1
            Dim b As Double = x1 - x0
            Dim c As Double = x0 * y1 - x1 * y0

            Dim v As Double = Sqrt(a * a + b * b)

            a /= v
            b /= v
            c /= v

            aa(i) = a
            bb(i) = b
            cc(i) = c

            If Abs(c) > hi Then
                hi = Abs(c)
            End If
        Next

        m = 2 * n + 4

        ReDim ha(m - 1)
        ReDim hb(m - 1)
        ReDim hc(m - 1)
        ReDim hk(m - 1)

        ha(0) = 1.0
        hb(0) = 0.0
        hc(0) = box
        hk(0) = 0.0

        ha(1) = -1.0
        hb(1) = 0.0
        hc(1) = box
        hk(1) = 0.0

        ha(2) = 0.0
        hb(2) = 1.0
        hc(2) = box
        hk(2) = 0.0

        ha(3) = 0.0
        hb(3) = -1.0
        hc(3) = box
        hk(3) = 0.0

        Dim p As Integer = 4

        For i As Integer = 0 To n - 1
            ha(p) = aa(i)
            hb(p) = bb(i)
            hc(p) = -cc(i)
            hk(p) = 1.0
            p += 1

            ha(p) = -aa(i)
            hb(p) = -bb(i)
            hc(p) = cc(i)
            hk(p) = 1.0
            p += 1
        Next

        Dim rng As New Random(239)

        For i As Integer = m - 1 To 5 Step -1
            Dim j As Integer = 4 + rng.Next(i - 4 + 1)
            swp(i, j)
        Next

        Dim lo As Double = 0.0
        Dim bx As Double = 0.0
        Dim by As Double = 0.0
        Dim tx As Double = 0.0
        Dim ty As Double = 0.0

        For it As Integer = 0 To 79
            Dim mid As Double = (lo + hi) / 2.0

            If ok(mid, tx, ty) Then
                hi = mid
                bx = tx
                by = ty
            Else
                lo = mid
            End If
        Next

        ok(hi + 0.0000001, bx, by)

        If bx < -box Then
            bx = -box
        ElseIf bx > box Then
            bx = box
        End If

        If by < -box Then
            by = -box
        ElseIf by > box Then
            by = box
        End If

        outs.WriteLine(
            bx.ToString("F12", CultureInfo.InvariantCulture) &
            " " &
            by.ToString("F12", CultureInfo.InvariantCulture)
        )

        outs.Flush()
    End Sub
End Module
