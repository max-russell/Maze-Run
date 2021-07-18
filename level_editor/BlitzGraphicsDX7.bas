Attribute VB_Name = "BlitzGraphicsDX7"
Option Explicit

Const Debug_Mode = False

'BLITZGRAPHICS.BAS MODULE (DIRECT X 7 VERSION)
'Imitates Blitz Basic graphics routines

'Start by using WindowGraphics with the PictureBox you want to use
'If the picture box can change size, do a ResizeGraphics (eg, in the
'Resize event for the form. Then draw the view again.
'Handles to images are not integers as Blitz, but variables of type 'ImageType'
'Use FreeImage to get rid of images when finished

'Subs:
'   WindowGraphics(Box as PictureBox, Wd, Ht)      Box is the picture box where you will do your drawing
'   ResizeGraphics(Wd,Ht)                          Call this if the picture box changes dimensions
'   EndGraphics                                    v All these are the same as Blitz
'   Img = CreateImage(w,h)
'   Img = LoadImage(file$)
'   MaskImage(col)
'   FreeImage(Img)
'   SetBuffer(Buffer)
'   Buffer = BackBuffer() / FrontBuffer() / ImageBuffer(Img)
'   GrabImage(Img, x, y)
'   ClsBuffer(col)                                       = Cls in Blitz
'   DrawImage(Img, x, y)
'   DrawBlock(Img, x, y)
'   DrawImageRect(Img, x, y, r_x, r_y, r_w,r_h)
'   DrawBlockRect(Img, x, y, r_x, r_y, r_w,r_h)
'   Flip()
'   Rect x, y, w, h, col, solid                    Draw a rectangle!
'   BlitzLine x1, y1, x2, y2, col                  Draw a line!

Declare Function GetPixel Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long) As Long
Declare Function GetObject Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long
Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Declare Function APILoadImage Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal un1 As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal un2 As Long) As Long
Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Declare Function CreateBitmap Lib "gdi32" (ByVal nWidth As Long, ByVal nHeight As Long, ByVal nPlanes As Long, ByVal nBitCount As Long, lpBits As Any) As Long
Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Declare Function FillRect Lib "user32.dll" (ByVal hdc As Long, lpRect As RECT, ByVal hbrush As Long) As Long
Declare Function CreateSolidBrush Lib "gdi32.dll" (ByVal crColor As Long) As Long
Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Declare Function OffsetRect Lib "user32.dll" (lpRect As RECT, ByVal X As Long, ByVal Y As Long) As Long
Declare Function SetRect Lib "user32" (lpRect As RECT, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Declare Function LockWindowUpdate Lib "user32" (ByVal hWnd As Long) As Long

Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Private Declare Function Rectangle Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Private Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long) As Long
Private Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, lpPoint As Any) As Long

Public Type Bitmap '14 bytes
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type

Public Type ImageType
    Surface As DirectDrawSurface7
    alive As Boolean
    MaskCol As Long
    Width As Long
    Height As Long
End Type

Private GraphicsBox As PictureBox 'Store a reference to the box where the graphics will go
Private GfxWd As Long '} Stores dimensions of front and back buffer view
Private GfxHt As Long '}

'DC for when we need to do ordinary API stuff
Private buf_spare As Long
Private bmp_sparedefault As Long

'Direct X Main Variables
Private DX As New DirectX7
Private DD As DirectDraw7
Private buf_back As DirectDrawSurface7
Private buf_front As DirectDrawSurface7
Private buf_Current As DirectDrawSurface7
Private buf_Current_W As Long
Private buf_Current_H As Long

'The first desciptor describes the screen
'The second describes offscreen surfaces including the back buffer
Private ddsd1 As DDSURFACEDESC2
Private ddsd2 As DDSURFACEDESC2
Private CKey As DDCOLORKEY

'The clipper handles obscured surfaces and stops our application
'from drawing over the top of other windows.
Private ddClipper As DirectDrawClipper

Private GraphicsInitialised As Boolean

Public Sub WindowGraphics(ByRef Box As PictureBox)
Dim r1 As RECT, bmp_dummy As Long, ddrval As Long

    If GraphicsInitialised = True Then
        EndGraphics
    End If
    
    'Initialization procedure
    'The empty string parameter means to use the active display driver
    Set DD = DX.DirectDrawCreate("")
    Call DD.SetCooperativeLevel(Box.hWnd, DDSCL_NORMAL)
    'Indicate that the ddsCaps member is valid in this type
    ddsd1.lFlags = DDSD_CAPS
    'This surface is the primary surface (what is visible to the user)
    ddsd1.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    'You're now creating the primary surface with the surface description you just set
    Set buf_front = DD.CreateSurface(ddsd1)
    'Creates the clipper, and attaches it to the picturebox and
    'the primary surface. This is all that has to be done - the clipper
    'itself handles everything else.
    Set ddClipper = DD.CreateClipper(0)
    ddClipper.SetHWnd Box.hWnd
    buf_front.SetClipper ddClipper

    Call DX.GetWindowRect(Box.hWnd, r1)
    GfxWd = r1.Right - r1.Left
    GfxHt = r1.Bottom - r1.Top
        
    'Now let's set the second surface description
    ddsd2.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    'This is going to be a plain off-screen surface - ie, to hold a bitmap
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsd2.lWidth = GfxWd
    ddsd2.lHeight = GfxHt
    Set buf_back = DD.CreateSurface(ddsd2)
    
    OffsetRect r1, -r1.Left, -r1.Top
    ddrval = buf_back.BltColorFill(r1, RGB(0, 0, 0))
    
    bmp_dummy = CreateCompatibleBitmap(Box.hdc, 1, 1)
    buf_spare = CreateCompatibleDC(Box.hdc)
    bmp_sparedefault = SelectObject(buf_spare, bmp_dummy)
    bmp_dummy = SelectObject(buf_spare, bmp_sparedefault)
    DeleteObject bmp_dummy

    'The current buffer is the back buffer by default
    Set buf_Current = buf_back
    buf_Current_W = GfxWd
    buf_Current_H = GfxHt
        
    GraphicsInitialised = True
    Set GraphicsBox = Box

End Sub

Public Sub ResizeGraphics()
    
Dim ChangeBufCurrent As Boolean, r1 As RECT, ddrval As Long
    
    'Use this if the size of the front buffer changes
    
    If buf_Current Is buf_back Then
        Set buf_Current = Nothing
        ChangeBufCurrent = True
    End If
    
    Set buf_back = Nothing
    
    Call DX.GetWindowRect(GraphicsBox.hWnd, r1)
    GfxWd = r1.Right - r1.Left
    GfxHt = r1.Bottom - r1.Top

    Set buf_back = Nothing
    ddsd2.lWidth = GfxWd
    ddsd2.lHeight = GfxHt
    Set buf_back = DD.CreateSurface(ddsd2)
    OffsetRect r1, -r1.Left, -r1.Top
    ddrval = buf_back.BltColorFill(r1, RGB(0, 0, 0))
    
    If ChangeBufCurrent = True Then
        Set buf_Current = buf_back
        buf_Current_W = GfxWd
        buf_Current_H = GfxHt
    End If
    
End Sub


Public Sub EndGraphics()
Dim img As ImageType, a As Long

    If GraphicsInitialised = False Then Exit Sub

    Set buf_front = Nothing
    Set buf_back = Nothing
    Set buf_Current = Nothing
    
    SelectObject buf_spare, bmp_sparedefault
    DeleteDC buf_spare
    
    GraphicsInitialised = False

End Sub

Public Function LoadImage(file As String) As ImageType

Dim bmp As Long, bmpinfo As Bitmap, r1 As RECT, ddrval As Long
Dim dc As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    
    Open file For Input As #1
    Close #1

    bmp = APILoadImage(ByVal 0&, file, 0, 0, 0, &H10)
    GetObject bmp, Len(bmpinfo), bmpinfo
    LoadImage.MaskCol = RGB(0, 0, 0)
    LoadImage.Width = bmpinfo.bmWidth
    LoadImage.Height = bmpinfo.bmHeight
        
    ddsd2.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsd2.lWidth = LoadImage.Width
    ddsd2.lHeight = LoadImage.Height
    Set LoadImage.Surface = DD.CreateSurface(ddsd2)
        
    SelectObject buf_spare, bmp
    dc = LoadImage.Surface.GetDC
    
    BitBlt dc, 0, 0, LoadImage.Width, LoadImage.Height, _
        buf_spare, 0, 0, vbSrcCopy
    CKey.low = LoadImage.MaskCol
    CKey.high = LoadImage.MaskCol
    LoadImage.Surface.SetColorKey DDCKEY_SRCBLT, CKey
        
    LoadImage.Surface.ReleaseDC dc
    SelectObject buf_spare, bmp_sparedefault
    DeleteObject bmp
        
    LoadImage.alive = True

End Function

Public Sub FreeImage(img As ImageType)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    If img.Surface Is buf_Current Then
        Set buf_Current = Nothing
    End If
        
    Set img.Surface = Nothing

    img.alive = False

End Sub

Public Function CreateImage(ByVal wd As Long, ByVal Ht As Long) As ImageType
Dim r As RECT, ddrval As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    ddsd2.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsd2.lWidth = wd
    ddsd2.lHeight = Ht
    Set CreateImage.Surface = DD.CreateSurface(ddsd2)
    CreateImage.Width = wd
    CreateImage.Height = Ht
    CreateImage.alive = True
    CreateImage.MaskCol = RGB(0, 0, 0)
    CKey.low = CreateImage.MaskCol
    CKey.high = CreateImage.MaskCol
    CreateImage.Surface.SetColorKey DDCKEY_SRCBLT, CKey
    r.Left = 0: r.Top = 0: r.Bottom = Ht: r.Right = wd
    
    ddrval = CreateImage.Surface.BltColorFill(r, RGB(0, 255, 0))
    If Debug_Mode = True Then
        Debug.Print "CreateImage.Surface.BltColorFill: " & DDError(ddrval)
    End If
    
End Function

Public Sub MaskImage(img As ImageType, ByVal col As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    img.MaskCol = col
    CKey.low = col
    CKey.high = col
    img.Surface.SetColorKey DDCKEY_SRCBLT, CKey

End Sub

Public Sub SetBuffer(buffer As DirectDrawSurface7)
Dim td As DDSURFACEDESC2

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    Set buf_Current = buffer
    buf_Current.GetSurfaceDesc td
    buf_Current_W = td.lWidth
    buf_Current_H = td.lHeight

End Sub

Public Function BackBuffer() As DirectDrawSurface7

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    Set BackBuffer = buf_back

End Function

Public Function FrontBuffer() As DirectDrawSurface7

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    Set FrontBuffer = buf_front

End Function

Public Function ImageBuffer(img As ImageType) As DirectDrawSurface7

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    
    Set ImageBuffer = img.Surface

End Function

Public Sub GrabImage(img As ImageType, ByVal X As Long, ByVal Y As Long)

'Dim buf_temp As Long, bmp_old As Long, dbitmap As Bitmap
Dim r As RECT, ddrval As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    If img.alive = True Then
    
        r.Left = X
        r.Top = Y
        r.Right = X + img.Width
        r.Bottom = Y + img.Height
        ddrval = img.Surface.BltFast(0, 0, buf_Current, r, DDBLTFAST_NOCOLORKEY)
        If Debug_Mode = True Then
            Debug.Print "GrabImage - BltFast: " & DDError(ddrval)
        End If
    Else
        MsgBox "Error: Image not initalised."
    End If

End Sub

Public Sub ClsBuffer(col As Long)
Dim ddrval As Long, r As RECT ', td As DDSURFACEDESC2

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'Same as Cls in BlitzBasic, except we pass the cls colour as an argument
    
    'buf_Current.GetSurfaceDesc td
    SetRect r, 0, 0, buf_Current_W, buf_Current_H
    
    ddrval = buf_Current.BltColorFill(r, col)
    If Debug_Mode = True Then
        Debug.Print "ClsBuffer - BltColorFill: " & DDError(ddrval)
    End If
    
End Sub

Public Sub BlitzRect(ByVal X As Long, ByVal Y As Long, ByVal Width As Long, ByVal Height As Long, ByVal col As Long, solid As Boolean)

Dim hbrush As Long ' receives handle to the blue hatched brush to use
Dim hPen As Long, oldPen As Long, dc As Long
Dim r As RECT ' rectangular area to fill

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    dc = buf_Current.GetDC
    If solid = True Then
        SetRect r, X, Y, X + Width, Y + Height
        hbrush = CreateSolidBrush(col)
        ' Fill in the desired rectangular area
        FillRect dc, r, hbrush ' fill the rectangle using the brush
        ' Delete the brush we created in order to free up resources
        DeleteObject hbrush
    Else
        hPen = CreatePen(0, 1, col)
        oldPen = SelectObject(dc, hPen)

        MoveToEx dc, X, Y, 0
        LineTo dc, X + Width, Y
        LineTo dc, X + Width, Y + Height
        LineTo dc, X, Y + Height
        LineTo dc, X, Y

        SelectObject dc, oldPen
        DeleteObject hPen
    End If
    buf_Current.ReleaseDC dc

End Sub

Sub BlitzLine(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long, col As Long)
Dim hPen As Long, oldPen As Long, dc As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    dc = buf_Current.GetDC

    hPen = CreatePen(0, 1, col)
    oldPen = SelectObject(dc, hPen)

    MoveToEx dc, X1, Y1, 0
    LineTo dc, X2, Y2
    SelectObject dc, oldPen
    DeleteObject hPen
    buf_Current.ReleaseDC dc

End Sub

Public Sub DrawImage(img As ImageType, ByVal X As Long, ByVal Y As Long)
Dim destrect As RECT, srcrect As RECT, ddrval As Long
    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    If img.alive = False Then
        MsgBox "Image not initialised!"
        Exit Sub
    End If

    SetRect srcrect, 0, 0, img.Width, img.Height

    'If the position is completely off the surface then just get out
    If X < 0 - img.Width Then Exit Sub
    If Y < 0 - img.Height Then Exit Sub
    If X > buf_Current_W Then Exit Sub
    If Y > buf_Current_H Then Exit Sub

    'If it's partially off the surface, adjust rectangle
    If X < 0 Then
        srcrect.Left = -X
        X = 0
    End If
    If Y < 0 And Y > -img.Height Then
        srcrect.Top = -Y
        Y = 0
    End If
    If X > buf_Current_W - img.Width Then
        srcrect.Right = srcrect.Right - (X + img.Width - buf_Current_W)
    End If
    If Y > buf_Current_H - img.Height Then
        srcrect.Bottom = srcrect.Bottom - (Y + img.Height - buf_Current_H)
    End If

    ddrval = buf_Current.BltFast(X, Y, img.Surface, srcrect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY)
        
    If Debug_Mode = True Then
        Debug.Print "DrawImage - BltFast: " & DDError(ddrval)
    End If

End Sub

Public Sub DrawBlock(img As ImageType, ByVal X As Long, ByVal Y As Long)
Dim destrect As RECT, srcrect As RECT, ddrval As Long
    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    If img.alive = False Then
        MsgBox "Image not initialised!"
        Exit Sub
    End If

    SetRect destrect, X, Y, X + img.Width, Y + img.Height
    SetRect srcrect, 0, 0, img.Width, img.Height

    ddrval = buf_Current.BltFast(X, Y, img.Surface, srcrect, DDBLTFAST_WAIT)
        
    If Debug_Mode = True Then
        Debug.Print "DrawImage - BltFast: " & DDError(ddrval)
    End If
End Sub

Public Sub DrawImageRect(img As ImageType, ByVal X As Long, ByVal Y As Long, ByVal r_x As Long, ByVal r_y As Long, ByVal r_w As Long, ByVal r_h As Long)
Dim srcrect As RECT, ddrval As Long
    
    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    If img.alive = False Then
        MsgBox "Image not initialised!"
        Exit Sub
    End If

    SetRect srcrect, r_x, r_y, r_x + r_w, r_y + r_h

    ddrval = buf_Current.BltFast(X, Y, img.Surface, srcrect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY)
        
    If Debug_Mode = True Then
        Debug.Print "DrawImage - BltFast: " & DDError(ddrval)
    End If

End Sub

Public Sub DrawBlockRect(img As ImageType, ByVal X As Long, ByVal Y As Long, ByVal r_x, ByVal r_y, ByVal r_w, ByVal r_h)
Dim srcrect As RECT, ddrval As Long
    
    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    If img.alive = False Then
        MsgBox "Image not initialised!"
        Exit Sub
    End If

    SetRect srcrect, r_x, r_y, r_x + r_w, r_y + r_h

    ddrval = buf_Current.BltFast(X, Y, img.Surface, srcrect, DDBLTFAST_WAIT)
        
    If Debug_Mode = True Then
        Debug.Print "DrawImage - BltFast: " & DDError(ddrval)
    End If
    
End Sub

Public Sub Flip()
Dim rsrc As RECT, rdst As RECT, ddrval As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    DD.RestoreAllSurfaces

    Call DX.GetWindowRect(GraphicsBox.hWnd, rdst)

    SetRect rsrc, 0, 0, GfxWd, GfxHt

    ddrval = buf_front.Blt(rdst, buf_back, rsrc, DDBLT_WAIT) ' Or DDBLT_ASYNC)

    If Debug_Mode = True Then
        Debug.Print "Flip - Blt: " & DDError(ddrval)
    End If

End Sub

Public Sub DrawToDC(img As ImageType, hdc As Long, ByVal X As Long, ByVal Y As Long)
Dim sr As RECT, dr As RECT

    If img.alive = False Then
        MsgBox "DrawToDC: Image does not exist!"
        Stop
        EndGraphics
    End If

    SetRect sr, 0, 0, img.Width, img.Height
    SetRect dr, X, Y, X + img.Width, Y + img.Height
    img.Surface.BltToDC hdc, sr, dr

End Sub

Public Sub DrawToDCRect(img As ImageType, hdc As Long, ByVal X As Long, ByVal Y As Long, ByVal r_x As Long, ByVal r_y As Long, ByVal r_w As Long, ByVal r_h As Long)
Dim sr As RECT, dr As RECT

    If img.alive = False Then
        MsgBox "DrawToDC: Image does not exist!"
        Stop
        EndGraphics
    End If

    SetRect sr, r_x, r_y, r_w + r_x, r_h + r_y
    SetRect dr, X, Y, X + r_w, Y + r_h
    img.Surface.BltToDC hdc, sr, dr

End Sub

'Public Function GetPicinBuffer(img As ImageType) As Long
'
'    If GraphicsInitialised = False Then
'        MsgBox "Error: Graphics not initialised."
'    End If
'
'    'GetObject img, Len(dbitmap), dbitmap
'    SelectObject buf_spare, img.bmp
'
'    GetPicinBuffer = buf_spare
'
'End Function


Public Function DDError(e As Long) As String

    Select Case e
    Case DD_OK
    DDError = "The request completed successfully."
    Case DDERR_CANTCREATEDC
    DDError = "Windows cannot create any more device contexts (DCs), or a DC was requested for a palette-indexed surface when the surface had no palette and the display mode was not palette-indexed (that is, DirectDraw cannot select a proper palette into the DC)."
    Case DDERR_CANTLOCKSURFACE
    DDError = "Access to this surface is refused because an attempt was made to lock the primary surface without DCI support."
    Case DDERR_CLIPPERISUSINGHWND
    DDError = "An attempt was made to set a clip list for a DirectDrawClipper object that is already monitoring a window handle."
    Case DDERR_COLORKEYNOTSET
    DDError = "No source color key is specified for this operation."
    Case DDERR_CURRENTLYNOTAVAIL
    DDError = "No support is currently available."
    Case DDERR_DCALREADYCREATED
    DDError = "A device context (DC) has already been returned for this surface. Only one DC can be retrieved for each surface."
    Case DDERR_DEVICEDOESNTOWNSURFACE
    DDError = "Surfaces created by one DirectDraw device cannot be used directly by another DirectDraw device."
    Case DDERR_DIRECTDRAWALREADYCREATED
    DDError = "A DirectDraw object representing this driver has already been created for this process."
    Case DDERR_EXCLUSIVEMODEALREADYSET
    DDError = "An attempt was made to set the cooperative level when it was already set to exclusive."
    Case DDERR_GENERIC
    DDError = "There is an undefined error condition."
    Case DDERR_HEIGHTALIGN
    DDError = "The height of the provided rectangle is not a multiple of the required alignment."
    Case DDERR_IMPLICITLYCREATED
    DDError = "The surface cannot be restored because it is an implicitly created surface."
    Case DDERR_INCOMPATIBLEPRIMARY
    DDError = "The primary surface creation request does not match the existing primary surface."
    Case DDERR_INVALIDCAPS
    DDError = "One or more of the capability bits passed to the callback function are incorrect."
    Case DDERR_INVALIDCLIPLIST
    DDError = "DirectDraw does not support the provided clip list."
    Case DDERR_INVALIDMODE
    DDError = "DirectDraw does not support the requested mode."
    Case DDERR_INVALIDOBJECT
    DDError = "DirectDraw received a pointer that was an invalid DirectDraw object."
    Case DDERR_INVALIDPARAMS
    DDError = "One or more of the parameters passed to the method are incorrect."
    Case DDERR_INVALIDPIXELFORMAT
    DDError = "The pixel format was invalid as specified."
    Case DDERR_INVALIDPOSITION
    DDError = "The position of the overlay on the destination is no longer valid."
    Case DDERR_INVALIDRECT
    DDError = "The provided rectangle was invalid."
    Case DDERR_LOCKEDSURFACES
    DDError = "One or more surfaces are locked, causing the failure of the requested operation."
    Case DDERR_MOREDATA
    DDError = "There is more data available than the specified buffer size can hold."
    Case DDERR_NOALPHAHW
    DDError = "No alpha acceleration hardware is present or available, causing the failure of the requested operation."
    Case DDERR_NOBLTHW
    DDError = "No blitter hardware is present."
    Case DDERR_NOCLIPLIST
    DDError = "No clip list is available."
    Case DDERR_NOCLIPPERATTACHED
    DDError = "No DirectDrawClipper object is attached to the surface object."
    Case DDERR_NOCOLORCONVHW
    DDError = "The operation cannot be carried out because no color-conversion hardware is present or available."
    Case DDERR_NOCOLORKEYHW
    DDError = "The operation cannot be carried out because there is no hardware support for the destination color key."
    Case DDERR_NOCOOPERATIVELEVELSET
    DDError = "A create function is called without the IDirectDraw::SetCooperativeLevel method being called."
    Case DDERR_NODC
    DDError = "No DC has been created for this surface."
    Case DDERR_NOFLIPHW
    DDError = "Flipping visible surfaces is not supported."
    Case DDERR_NOOVERLAYDEST
    DDError = "The IDirectDrawSurface::GetOverlayPosition method is called on an overlay but the IDirectDrawSurface::UpdateOverlay method has not been called on to establish a destination."
    Case DDERR_NOOVERLAYHW
    DDError = "The operation cannot be carried out because no overlay hardware is present or available."
    Case DDERR_NOPALETTEATTACHED
    DDError = "No palette object is attached to this surface."
    Case DDERR_NOPALETTEHW
    DDError = "There is no hardware support for 16- or 256-color palettes."
    Case DDERR_NORASTEROPHW
    DDError = "The operation cannot be carried out because no appropriate raster operation hardware is present or available."
    Case DDERR_NOSTRETCHHW
    DDError = "The operation cannot be carried out because there is no hardware support for stretching."
    Case DDERR_NOTAOVERLAYSURFACE
    DDError = "An overlay component is called for a non-overlay surface."
    Case DDERR_NOTFLIPPABLE
    DDError = "An attempt has been made to flip a surface that cannot be flipped."
    Case DDERR_NOTFOUND
    DDError = "The requested item was not found."
    Case DDERR_NOTLOCKED
    DDError = "An attempt is made to unlock a surface that was not locked."
    Case DDERR_NOTPALETTIZED
    DDError = "The surface being used is not a palette-based surface."
    Case DDERR_NOVSYNCHW
    DDError = "The operation cannot be carried out because there is no hardware support for vertical blank synchronized operations."
    Case DDERR_NOZOVERLAYHW
    DDError = "Not supported."
    Case DDERR_OUTOFCAPS
    DDError = "The hardware needed for the requested operation has already been allocated."
    Case DDERR_OUTOFMEMORY
    DDError = "DirectDraw does not have enough memory to perform the operation."
    Case DDERR_OUTOFVIDEOMEMORY
    DDError = "DirectDraw does not have enough display memory to perform the operation."
    Case DDERR_OVERLAPPINGRECTS
    DDError = "Operation could not be carried out because the source and destination rectangles are on the same surface and overlap each other."
    Case DDERR_OVERLAYNOTVISIBLE
    DDError = "The IDirectDrawSurface::GetOverlayPosition method is called on a hidden overlay."
    Case DDERR_PALETTEBUSY
    DDError = "Access to this palette is refused because the palette is locked by another thread."
    Case DDERR_PRIMARYSURFACEALREADYEXISTS
    DDError = "This process has already created a primary surface."
    Case DDERR_REGIONTOOSMALL
    DDError = "The region passed to the IDirectDrawClipper::GetClipList method is too small."
    Case DDERR_SURFACEBUSY
    DDError = "Access to the surface is refused because the surface is locked by another thread."
    Case DDERR_SURFACELOST
    DDError = "Access to the surface is refused because the surface memory is gone. Call the IDirectDrawSurface::Restore method on this surface to restore the memory associated with it."
    Case DDERR_TOOBIGHEIGHT
    DDError = "The height requested by DirectDraw is too large."
    Case DDERR_TOOBIGSIZE
    DDError = "The size requested by DirectDraw is too large. However, the individual height and width are valid sizes."
    Case DDERR_TOOBIGWIDTH
    DDError = "The width requested by DirectDraw is too large."
    Case DDERR_UNSUPPORTED
    DDError = "The operation is not supported."
    Case DDERR_UNSUPPORTEDFORMAT
    DDError = "The pixel format requested is not supported by DirectDraw."
    Case DDERR_VERTICALBLANKINPROGRESS
    DDError = "A vertical blank is in progress."
    Case DDERR_VIDEONOTACTIVE
    DDError = "The video port is not active."
    Case DDERR_WASSTILLDRAWING
    DDError = "The previous blit operation that is transferring information to or from this surface is incomplete."
    Case DDERR_WRONGMODE
    DDError = "This surface cannot be restored because it was created in a different mode."
    End Select

End Function
