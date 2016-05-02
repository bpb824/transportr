
color_palette = function(n,type="cat",palette = "Nelson Nygaard"){

  if(palette=="Nelson Nygaard"){
    color_set = list()
    color_set$blue = "#00659A"
    color_set$red = "#D46F4E"
    color_set$orange="#EEAC5B"
    color_set$citron ="#BDBD00"
    color_set$cocoa="#987366"
    color_set$sand=colorspace::hex2RGB("#CEC8A8")
    color_set$plum=colorspace::hex2RGB("#6E2B62")
    if(type=="cat"){
      all_colors = c(color_set$blue,color_set$red,
                     color_set$citron,color_set$plum,
                     color_set$orange,color_set$cocoa)

    }else if (type=="seq"){

    }else if (type=="div"){
      all_colors = c(color_set$blue,color_set$plum,
                     color_set$red,color_set$orange,
                     color_set$orange,color_set$sand,color_set$cocoa)

    }
  }

  else if(palette=="MnDOT"){
    color_set = list()
    color_set$orange = rgb(246/255,144/255,92/255)
    color_set$process_blue = rgb(4/255,159/255,218/255)
    color_set$green= rgb(0/255,177/255,89/255)
    color_set$blue = rgb(2/255,63/255,94/255)
    color_set$tangerine = rgb(250/255,166/255,51/255)
    color_set$gray_blue = rgb(116/255,129/255,190/255)
    color_set$red = rgb(238/255,52/255,35/255)
    color_set$green = rgb(0/255,89/255,83/255)
    if(type=="cat"){
      colors = unlist(color_set)
    }else if (type=="seq"){

    }else if (type=="div"){

    }
  }
  
  available = length(colors)
  indices = seq(1,n)

  return(colors[indices])

}
