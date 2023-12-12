//
// Project Name: Image Processing in F#
// Project Description: This program asks user for image filename. 
// There are several options to choose from what do user want to do with the image and
// then the program generates an image based on the option selected.
// Student Name: Pratik Patel
// Student Netid: ppate460
// Date Submitted: Oct. 26th, 2023
// Due Date: Oct. 28th, 2023
// More details about library: There are five function which performs actions on the image user selected. 
// None of the functions uses mutable, arrays, loops, and direct image manipulation method.
//

namespace ImageLibrary

module Operations =

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value. Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation. A normal average
  // (adding the three values and dividing by 3) is NOT the best,
  // since the human eye does not perceive the brightness of
  // red, green and blue the same. The human eye perceives
  // green as brighter than red and it perceived red as brighter
  // than blue. Research has shown that the following weighted
  // values should be used when calculating grayscale.
  // - the green value should account for 58.7% of the grayscale amount.
  // - the red value should account for 29.9% of the grayscale amount.
  // - the blue value should account for 11.4% of the grayscale amount.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    //Calculating the grayscale value of the image
    let GrayscalePixel (red, green, blue) = 
      let grayValue = int (float red * 0.299 + float green * 0.587 + float blue * 0.114)
      (grayValue, grayValue, grayValue)

    //Recursively apply grayscale to each pixel in the image
    let rec ApplyGrayscale img = 
      match img with
      | [] -> []   
      | row :: leftoverRow ->
        List.map GrayscalePixel row :: ApplyGrayscale leftoverRow

    //COnstructng an image
    let finalImage = ApplyGrayscale image

    //returning an image
    finalImage


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values
  // become darker and light values become lighter. Given a
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0.
  //
  // Example: if threshold is 100 and depth is 255, then given
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 

    //applying threshold to each and every pixel individually
    let ApplyThresholdToPixel (red, green, blue) = 
      let ThresholdRed = if red > threshold then depth else 0
      let ThresholdGreen = if green > threshold then depth else 0
      let ThresholdBlue = if blue > threshold then depth else 0
      (ThresholdRed, ThresholdGreen, ThresholdBlue)

    //Recursiely apply Threshold to each pixel in the image
    let rec ApplyThresholdToImage img = 
      match img with 
      | [] -> []
      | row :: leftoverImage ->
        List.map ApplyThresholdToPixel row :: ApplyThresholdToImage leftoverImage

    //Constructing a new inage
    let finalImage = ApplyThresholdToImage image
     
    //Returning a finalImage
    finalImage


  //
  // FlipHorizontal: 
  //
  // Flips an image so that what’s on the left is now on
  // the right, and what’s on the right is now on the left.
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 

    //Higer-order function and recursion to flip the row of the image
    let FlipRow row = List.rev row

    let finalImage = List.map FlipRow image
    
    //return the image
    finalImage


  //
  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an
  // "edge" in the original image. If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels.
  // We only compares each pixel in the image with the
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  // sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an
  // integer 0 < threshold < 255. If the color distance between
  // the original pixel either of the two neighboring pixels
  // is greater than the threshold amount, an edge occurs and
  // a black pixel is put in the resulting image at the location
  // of the original pixel.
  //
  // Returns: updated image.
  //
  let EdgeDetect (width:int)
                 (height:int)
                 (depth:int)
                 (image:(int*int*int) list list)
                 (threshold:int) =
    
    //Calculates the distance between two pixels
    let DistanceDifference (pixel1: int*int*int) (pixel2: int*int*int) =
        let squaredPixel x = x * x
        let (red1, green1, blue1) = pixel1
        let (red2, green2, blue2) = pixel2
        //Pythagorean Theroem 
        sqrt (float (squaredPixel (red1 - red2) + squaredPixel (green1 - green2) + squaredPixel (blue1 - blue2) ) )

    // Whether a pixel is at an edge or not
    let isPixelanEdge (x, y) =
        let pixel = List.nth (List.nth image y) x
        let rightPixel = if x + 1 < width then List.nth (List.nth image y) (x + 1) else pixel
        let bottomPixel = if y + 1 < height then List.nth (List.nth image (y + 1)) x else pixel

        //finding the difference from pixel to rightPixel and to bottomPixel
        let difference1 = DistanceDifference pixel rightPixel
        let difference2 = DistanceDifference pixel bottomPixel

        //if difference1 > threshold then true, otherwise false
        difference1 > float threshold || difference2 > float threshold

    //converting into color pixel
    let boolToPixel isPixelanEdge =
        if isPixelanEdge then (0, 0, 0) else (255, 255, 255)

    //processing a row of pixels using tail-recursion
    let rec processRow y = function
        | [] -> []
        | (x, pixel) :: leftover when x < width - 1 ->    //skip the right most column
          let pixelEdge = isPixelanEdge (x, y)
          let newPixel = boolToPixel pixelEdge
          newPixel :: processRow y leftover
        | _ :: leftover -> [] 

    //Processing an entire image
    let rec processImage y = function
        | [] -> []
        | row :: leftoverRow when y < height - 1 ->    //skip the last row
          let newRow = processRow y row
          newRow :: processImage (y + 1) leftoverRow
        | _ :: leftoverRow -> processImage (y + 1) leftoverRow 

    //Start processing an image from top-left corner using Higher order function
    let finalImage = processImage 0 (List.map (List.mapi (fun x p -> (x, p))) image)

    //returning final image
    finalImage


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 

    // Transfer rows and colms of the image
    let transferRowsColm image =
      //recursively performing the positions of rows and colms
      let rec transposeColms rows =
          match rows with
          | [] -> []
          | []::_ -> []
          | _ -> 
                List.map List.head rows :: transposeColms (List.map List.tail rows)
      transposeColms image

    //reverse the order of the rows in the image
    let reverseRows image =
        List.map List.rev image

    //construct the image by rotating 90 degree to right
    let finalImage =
        image
        |> transferRowsColm
        |> reverseRows

    //returning the image
    finalImage
