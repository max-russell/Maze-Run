Attribute VB_Name = "BlitzGraphics"
Option Explicit

'BLITZGRAPHICS.BAS MODULE
'Imitates Blitz Basic graphics routines

'Start by using WindowGraphics with the PictureBox you want to use
'If the picture box can change size, do a ResizeGraphics (eg, in the
'Resize event for the form. Then draw the view again.
'Handles to images are not integers as Blitz, but variables of type 'ImageType'
'Use FreeImage to get rid of images when finished, but the module
'should keep track of them and delete them when EndGraphics is called.

'Subs:
'   WindowGraphics(Box as PictureBox, Wd, Ht)      Box is the picture box where you will do your drawing
'   ResizeGraphics(Wd,Ht)                          Call this if the picture box changes dimensions
'   EndGraphics                                    v All these are the same as Blitz
'   Img = CreateImage(w,h)
'   Img = LoadImage(file$)
'   MaskImage(col)
'   FreeImage(Img)
'   SetBuffer(Buffer)
'   Buffer = BackBuffer() / FrontBuffer() / ImageBuffer(Img) / DCBuffer(hdc)
'   GrabImage(Img, x, y)
'   ClsBuffer(col)                                       = Cls in Blitz
'   DrawImage(Img, x, y)
'   DrawBlock(Img, x, y)
'   DrawImageRect(Img, x, y, r_x, r_y, r_w,r_h)
'   DrawBlockRect(Img, x, y, r_x, r_y, r_w,r_h)
'   Flip()
'   Rect x, y, w, h, col, solid                    Draw a rectangle!
'   BlitzLine x1, y1, x2, y2, col                  Draw a line!



'Declare Function TransparentBlt Lib "msimg32" _
'                (ByVal hdcDest As Long, ByVal nXOriginDest As Long, _
'                  ByVal nYOriginDest As Long, ByVal nWidthDest As Long, _
'                  ByVal nHeightDest As Long, ByVal hdcSrc As Long, _
'                  ByVal nXOriginSrc As Long, ByVal nYOriginSrc As Long, _
'                 ByVal nWidthSrc As Long, ByVal nHeightSrc As Long, _
'                 ByVal crTransparent As Long) As Long

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
Declare Function FillRect Lib "user32.dll" (ByVal hdc As Long, lpRect As Rect, ByVal hbrush As Long) As Long
Declare Function CreateSolidBrush Lib "gdi32.dll" (ByVal crColor As Long) As Long
Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long

Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Private Declare Function Rectangle Lib "gdi32" (ByVal hdc As Long, ByVal x1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long) As Long
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
    alive As Boolean
    bmp As Long
    bmpinfo As Bitmap
    MaskCol As Long
End Type

Private Type Rect
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Private GraphicsBox As PictureBox 'Store a reference to the box where the graphics will go
Private ImageTracker() As ImageType
Private ImagesTracked As Long

Private GfxWd As Long '} Stores dimensions of front and back buffer view
Private GfxHt As Long '}
Private buf_back As Long
'Private buf_front As Long
Private buf_Image As Long
Private buf_spare As Long
Private buf_Current As Long
Private bmp_imgdefault As Long
Private bmp_imgcurrent As Long
Private bmp_back As Long
Private bmp_backdefault As Long
Private bmp_sparedefault As Long

Private GraphicsInitialised As Boolean

Public Sub TransparentBlt(hDestDC As Long, lDestX As Long, lDestY As Long, lWidth As Long, lHeight As Long, hSourceDC As Long, lSourceX As Long, lSourceY As Long, lTransColor As Long)
'   This function copies a bitmap from one device context to the other
'   where every pixel in the source bitmap that matches the specified color
'   becomes transparent, letting the destination bitmap show through.

    Dim lOldColor As Long
    Dim hMaskDC As Long
    Dim hMaskBmp As Long
    Dim hOldMaskBmp As Long
    Dim hTempBmp As Long
    Dim hTempDC As Long
    Dim hOldTempBmp As Long
    Dim hDummy As Long
    Dim lRet As Long

    '   The Background colors of Source and Destination DCs must
    '   be the transparancy color in order to create a mask.
    lOldColor = SetBkColor&(hSourceDC, lTransColor)
    lOldColor = SetBkColor&(hDestDC, lTransColor)
    
    '   The mask DC must be compatible with the destination dc,
    '   but the mask has to be created as a monochrome bitmap.
    '   For this reason, we create a compatible dc and bitblt
    '   the mono mask into it.
    
    '   Create the Mask DC, and a compatible bitmap to go in it.
    hMaskDC = CreateCompatibleDC(hDestDC)
    hMaskBmp = CreateCompatibleBitmap(hDestDC, lWidth, lHeight)
    '   Move the Mask bitmap into the Mask DC
    hOldMaskBmp = SelectObject(hMaskDC, hMaskBmp)
    
    '   Create a monochrome bitmap that will be the actual mask bitmap.
    hTempBmp = CreateBitmap(lWidth, lHeight, 1, 1, 0&)
    '   Create a temporary DC, and put the mono bitmap into it
    hTempDC = CreateCompatibleDC(hDestDC)
    hOldTempBmp = SelectObject(hTempDC, hTempBmp)
    
    '   BitBlt the Source image into the mono dc to create a mono mask.
    If BitBlt(hTempDC, 0, 0, lWidth, lHeight, hSourceDC, lSourceX, lSourceY, vbSrcCopy) Then
        '   Copy the mono mask into our Mask DC
        hDummy = BitBlt(hMaskDC, 0, 0, lWidth, lHeight, hTempDC, 0, 0, vbSrcCopy)
    End If
    
    '   Clean up temp DC and bitmap
    hTempBmp = SelectObject(hTempDC, hOldTempBmp)
    hDummy = DeleteObject(hTempBmp)
    hDummy = DeleteDC(hTempDC)
    
    '   Copy the source to the destination with XOR
    lRet = BitBlt(hDestDC, lDestX, lDestY, lWidth, lHeight, hSourceDC, lSourceX, lSourceY, vbSrcInvert)
    '   Copy the Mask to the destination with AND
    lRet = BitBlt(hDestDC, lDestX, lDestY, lWidth, lHeight, hMaskDC, 0, 0, vbSrcAnd)
    '   Again, copy the source to the destination with XOR
    lRet = BitBlt(hDestDC, lDestX, lDestY, lWidth, lHeight, hSourceDC, lSourceX, lSourceY, vbSrcInvert)

    '   Clean up mask DC and bitmap
    hMaskBmp = SelectObject(hMaskDC, hOldMaskBmp)
    hDummy = DeleteObject(hMaskBmp)
    hDummy = DeleteDC(hMaskDC)

End Sub

Public Sub WindowGraphics(ByRef Box As PictureBox, ByVal wd As Long, ByVal ht As Long)
Dim bmp_dummy As Long

    If GraphicsInitialised = True Then
        EndGraphics
    End If

    'Set front buffer to the dc we have chosen
    'buf_front = Box.hdc
        
    'Create the back buffer, and make a bitmap for it that's the same
    'size as the window we're using for graphics. Select the bitmap
    'into the back buffer and keep the default bitmap from it so we
    'can switch it back in when we destroy the buffer once we've finished
    buf_back = CreateCompatibleDC(Box.hdc)
    bmp_back = CreateCompatibleBitmap(Box.hdc, wd, ht)
    bmp_backdefault = SelectObject(buf_back, bmp_back)
        
    GfxWd = wd
    GfxHt = ht
        
    bmp_dummy = CreateCompatibleBitmap(Box.hdc, 1, 1)
        
    buf_spare = CreateCompatibleDC(Box.hdc)
    bmp_sparedefault = SelectObject(buf_spare, bmp_dummy)
    bmp_dummy = SelectObject(buf_spare, bmp_sparedefault)
        
    ' Do the image buffer like the back buffer - but we select different
    ' bitmaps into the image buffer later.
    buf_Image = CreateCompatibleDC(Box.hdc)
    ' Select a dummy bitmap into the dc, just so we can store the default
    ' bitmap that was made when the imagebuffer was made.
    bmp_imgdefault = SelectObject(buf_Image, bmp_dummy)
    bmp_dummy = SelectObject(buf_Image, bmp_imgdefault)
        
    DeleteObject bmp_dummy
        
    'The current buffer is the back buffer by default
    buf_Current = buf_back
        
    GraphicsInitialised = True
    Set GraphicsBox = Box

End Sub

Public Sub ResizeGraphics(ByVal wd As Long, ByVal ht As Long)
    
Dim ChangeBufCurrent As Boolean
    
    'Use this if the size of the front buffer changes
    
    If buf_Current = buf_back Then
        ChangeBufCurrent = True
    End If
    
    'Put default bitmap back into back buffer, and delete image and dc
    SelectObject buf_back, bmp_backdefault
    DeleteObject bmp_back
    'DeleteDC buf_back
    bmp_back = CreateCompatibleBitmap(GraphicsBox.hdc, wd, ht)
    bmp_backdefault = SelectObject(buf_back, bmp_back)
    
    GfxWd = wd
    GfxHt = ht
    
    If ChangeBufCurrent = True Then buf_Current = buf_back
    
End Sub


Public Sub EndGraphics()
Dim img As ImageType, a As Long

    If GraphicsInitialised = False Then Exit Sub

    'Put default bitmap back into back buffer, and delete image and dc
    SelectObject buf_back, bmp_backdefault
    DeleteObject bmp_back
    DeleteDC buf_back

    'Put default bitmap back into image buffer and delete dc
    SelectObject buf_Image, bmp_imgdefault
    DeleteDC buf_Image
    
    SelectObject buf_spare, bmp_sparedefault
    DeleteDC buf_spare
    
    'Make sure we delete any bmps that are still around
    For a = 1 To ImagesTracked
        If ImageTracker(a).alive = True Then
            DeleteObject ImageTracker(a).bmp
        End If
    Next
    ReDim ImageTracker(0)
    ImagesTracked = 0
    
    GraphicsInitialised = False

End Sub

Public Function LoadImage(file As String) As ImageType

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    LoadImage.bmp = APILoadImage(ByVal 0&, file, 0, 0, 0, &H10)
    GetObject LoadImage.bmp, Len(LoadImage.bmpinfo), LoadImage.bmpinfo
    LoadImage.alive = True
    LoadImage.MaskCol = RGB(0, 0, 0)
    
    ImagesTracked = ImagesTracked + 1
    ReDim Preserve ImageTracker(ImagesTracked)
    ImageTracker(ImagesTracked) = LoadImage

End Function

Public Sub FreeImage(img As ImageType)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'If the image is selected into the current buffer then free it
    If img.bmp = bmp_imgcurrent Then
        SelectObject buf_Image, bmp_imgdefault
        bmp_imgcurrent = 0
    End If
    DeleteObject img.bmp
    img.alive = False
    
End Sub

Public Function CreateImage(ByVal wd As Long, ByVal ht As Long) As ImageType

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    CreateImage.bmp = CreateCompatibleBitmap(GraphicsBox.hdc, wd, ht)
    GetObject CreateImage.bmp, Len(CreateImage.bmpinfo), CreateImage.bmpinfo
    CreateImage.alive = True
    CreateImage.MaskCol = RGB(0, 0, 0)
    
    ImagesTracked = ImagesTracked + 1
    ReDim Preserve ImageTracker(ImagesTracked)
    ImageTracker(ImagesTracked) = CreateImage
    
End Function

Public Sub MaskImage(img As ImageType, ByVal col As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    img.MaskCol = col

End Sub

Public Sub SetBuffer(buffer As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    buf_Current = buffer

End Sub

Public Function BackBuffer() As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    SelectObject buf_Image, bmp_imgdefault
    BackBuffer = buf_back

End Function

Public Function FrontBuffer() As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    SelectObject buf_Image, bmp_imgdefault
    FrontBuffer = GraphicsBox.hdc 'buf_front

End Function

Public Function ImageBuffer(img As ImageType) As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'This selects the image into the image buffer and then
    'returns that selfsame buffer
    
    If bmp_imgcurrent <> img.bmp Then
        SelectObject buf_Image, img.bmp
        bmp_imgcurrent = img.bmp
    End If
    
    ImageBuffer = buf_Image

End Function

Public Sub GrabImage(img As ImageType, ByVal X As Long, ByVal Y As Long)

Dim buf_temp As Long, bmp_old As Long, dbitmap As Bitmap

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'Grab from the current buffer and draw it into the image.
    If img.bmp <> bmp_imgcurrent Then
        'If img is NOT already loaded into the image buffer, we use the
        'spare buffer
        SelectObject buf_spare, img.bmp
    
        If BitBlt(buf_spare, 0, 0, img.bmpinfo.bmWidth, img.bmpinfo.bmHeight, _
                buf_Current, X, Y, vbSrcCopy) = 0 Then
            MsgBox "Error doing BitBlt in GrabImage!"
            Stop
        End If
        SelectObject buf_spare, bmp_sparedefault
        
    Else
        If BitBlt(buf_Image, 0, 0, img.bmpinfo.bmHeight, img.bmpinfo.bmWidth, _
                buf_Current, X, Y, vbSrcCopy) = 0 Then
            MsgBox "Error doing BitBlt in GrabImage!"
            Stop
        End If
    
    End If

End Sub

Public Sub ClsBuffer(col As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'Same as Cls in BlitzBasic, except we pass the cls colour as an argument
    
    Dim hbrush As Long ' receives handle to the blue hatched brush to use
    Dim r As Rect ' rectangular area to fill
    Dim retval As Long ' return value

    ' Set the coordinates of the rectangle r
    r.Left = 0: r.Top = 0: r.Right = GfxWd: r.Bottom = GfxHt
    
    hbrush = CreateSolidBrush(col)
    ' Fill in the desired rectangular area
    retval = FillRect(buf_Current, r, hbrush) ' fill the rectangle using the brush
    ' Delete the brush we created in order to free up resources
    retval = DeleteObject(hbrush)

End Sub

Public Sub Rect(ByVal X As Long, ByVal Y As Long, ByVal width As Long, ByVal height As Long, ByVal col As Long, solid As Boolean)

Dim hbrush As Long ' receives handle to the blue hatched brush to use
Dim hPen As Long, oldPen As Long
Dim r As Rect ' rectangular area to fill
Dim retval As Long ' return value

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    
    If solid = True Then
        r.Left = X: r.Top = Y: r.Right = X + width: r.Bottom = Y + height
        hbrush = CreateSolidBrush(col)
        ' Fill in the desired rectangular area
        retval = FillRect(buf_Current, r, hbrush) ' fill the rectangle using the brush
        ' Delete the brush we created in order to free up resources
        retval = DeleteObject(hbrush)
    Else
    
        hPen = CreatePen(0, 1, col)
        oldPen = SelectObject(buf_Current, hPen)
        'Rectangle buf_Current, x, y, x + width, y + height
        
        MoveToEx buf_Current, X, Y, 0
        LineTo buf_Current, X + width, Y
        LineTo buf_Current, X + width, Y + height
        LineTo buf_Current, X, Y + height
        LineTo buf_Current, X, Y
        
        SelectObject buf_Current, oldPen
        DeleteObject hPen
    End If

End Sub

Sub BlitzLine(ByVal x1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long, col As Long)
Dim hPen As Long, oldPen As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If
    
    hPen = CreatePen(0, 1, col)
    oldPen = SelectObject(buf_Current, hPen)
    
    MoveToEx buf_Current, x1, y1, 0
    LineTo buf_Current, x2, y2
    SelectObject buf_Current, oldPen
    DeleteObject hPen

End Sub

Public Sub DrawImage(img As ImageType, ByVal X As Long, ByVal Y As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'GetObject img, Len(dbitmap), dbitmap
    SelectObject buf_spare, img.bmp

    'Draw img to the current buffer without transparency
    TransparentBlt buf_Current, X, Y, img.bmpinfo.bmWidth, img.bmpinfo.bmHeight, _
            buf_spare, 0, 0, img.MaskCol
        'MsgBox "Error doing BitBlt in DrawBlock!"
        'Stop
    'End If
    
    SelectObject buf_spare, bmp_sparedefault

End Sub

Public Sub DrawBlock(img As ImageType, ByVal X As Long, ByVal Y As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'GetObject img, Len(dbitmap), dbitmap
    SelectObject buf_spare, img.bmp

    'Draw img to the current buffer without transparency
    If BitBlt(buf_Current, X, Y, img.bmpinfo.bmWidth, img.bmpinfo.bmHeight, _
            buf_spare, 0, 0, vbSrcCopy) = 0 Then
        'MsgBox "Error doing BitBlt in DrawBlock!"
        'Stop
    End If
    
    SelectObject buf_spare, bmp_sparedefault
    
End Sub

Public Sub DrawImageRect(img As ImageType, ByVal X As Long, ByVal Y As Long, ByVal r_x As Long, ByVal r_y As Long, ByVal r_w As Long, ByVal r_h As Long)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'GetObject img, Len(dbitmap), dbitmap
    SelectObject buf_spare, img.bmp

    'Draw img to the current buffer with transparency
    TransparentBlt buf_Current, X, Y, r_w, r_h, buf_spare, r_x, r_y, img.MaskCol ') = 0 Then
    '    MsgBox "Error doing BitBlt in DrawImageRect!"
    '    Stop
    'End If
    
    SelectObject buf_spare, bmp_sparedefault

End Sub

Public Sub DrawBlockRect(img As ImageType, ByVal X As Long, ByVal Y As Long, ByVal r_x, ByVal r_y, ByVal r_w, ByVal r_h)

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'GetObject img, Len(dbitmap), dbitmap
    SelectObject buf_spare, img.bmp

    'Draw img to the current buffer without transparency
    If BitBlt(buf_Current, X, Y, r_w, r_h, _
            buf_spare, r_x, r_y, vbSrcCopy) = 0 Then
        MsgBox "Error doing BitBlt in DrawBlock!"
        Stop
    End If
    
    SelectObject buf_spare, bmp_sparedefault

End Sub

Public Sub Flip()

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'Draw img to the current buffer without transparency
    If BitBlt(GraphicsBox.hdc, 0, 0, GfxWd, GfxHt, _
            buf_back, 0, 0, vbSrcCopy) = 0 Then
        'MsgBox "Error doing BitBlt in Flip!: " & Err.LastDllError
        'Stop
    End If

End Sub

Public Function GetPicinBuffer(img As ImageType) As Long

    If GraphicsInitialised = False Then
        MsgBox "Error: Graphics not initialised."
    End If

    'GetObject img, Len(dbitmap), dbitmap
    SelectObject buf_spare, img.bmp
    
    GetPicinBuffer = buf_spare
    
End Function
