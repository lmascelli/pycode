import matplotlib.pyplot as plt
v2 = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ]
plt.figure()
plt.title("v2")
plt.plot(v2)
v3 = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.06279085, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06279052, 0.12533323, 0.18738131, 0.2486899, 0.309017, 0.36812454, 0.4257793, 0.4817537, 0.5358268, 0.58778524, 0.63742405, 0.6845471, 0.7289687, 0.7705133, 0.809017, 0.8443279, 0.8763067, 0.90482706, 0.9297765, 0.95105654, 0.96858317, 0.9822873, 0.9921147, 0.9980267, 1, 0.9980267, 0.99211466, 0.9822872, 0.96858317, 0.95105654, 0.9297765, 0.904827, 0.8763066, 0.84432787, 0.8090169, 0.77051324, 0.7289686, 0.68454707, 0.63742393, 0.5877852, 0.5358269, 0.48175356, 0.42577937, 0.3681244, 0.30901703, 0.2486899, 0.1873813, 0.1253332, 0.06279046, -0.00000008742278, -0.06279063, -0.12533337, -0.18738124, -0.24869007, -0.30901697, -0.36812478, -0.4257793, -0.4817535, -0.53582686, -0.5877851, -0.6374241, -0.68454707, -0.72896874, -0.77051336, -0.8090168, -0.84432805, -0.8763066, -0.9048272, -0.92977643, -0.9510566, -0.96858317, -0.9822872, -0.9921147, -0.9980267, -1, -0.9980267, -0.9921147, -0.9822872, -0.9685831, -0.9510565, -0.92977643, -0.9048272, -0.8763066, -0.8443278, -0.8090171, -0.77051336, -0.7289684, -0.6845468, -0.63742405, -0.5877853, -0.53582644, -0.4817537, -0.42577928, -0.36812454, -0.3090165, -0.24868982, -0.18738122, -0.12533312, -0.06279085, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06279052, 0.12533323, 0.18738131, 0.2486899, 0.309017, 0.36812454, 0.4257793, 0.4817537, 0.5358268, 0.58778524, 0.63742405, 0.6845471, 0.7289687, 0.7705133, 0.809017, 0.8443279, 0.8763067, 0.90482706, 0.9297765, 0.95105654, 0.96858317, 0.9822873, 0.9921147, 0.9980267, 1, 0.9980267, 0.99211466, 0.9822872, 0.96858317, 0.95105654, 0.9297765, 0.904827, 0.8763066, 0.84432787, 0.8090169, 0.77051324, 0.7289686, 0.68454707, 0.63742393, 0.5877852, 0.5358269, 0.48175356, 0.42577937, 0.3681244, 0.30901703, 0.2486899, 0.1873813, 0.1253332, 0.06279046, -0.00000008742278, -0.06279063, -0.12533337, -0.18738124, -0.24869007, -0.30901697, -0.36812478, -0.4257793, -0.4817535, -0.53582686, -0.5877851, -0.6374241, -0.68454707, -0.72896874, -0.77051336, -0.8090168, -0.84432805, -0.8763066, -0.9048272, -0.92977643, -0.9510566, -0.96858317, -0.9822872, -0.9921147, -0.9980267, -1, -0.9980267, -0.9921147, -0.9822872, -0.9685831, -0.9510565, -0.92977643, -0.9048272, -0.8763066, -0.8443278, -0.8090171, -0.77051336, -0.7289684, -0.6845468, -0.63742405, -0.5877853, -0.53582644, -0.4817537, -0.42577928, -0.36812454, -0.3090165, -0.24868982, -0.18738122, -0.12533312, -0.06279085, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06279052, 0.12533323, 0.18738131, 0.2486899, 0.309017, 0.36812454, 0.4257793, 0.4817537, 0.5358268, 0.58778524, 0.63742405, 0.6845471, 0.7289687, 0.7705133, 0.809017, 0.8443279, 0.8763067, 0.90482706, 0.9297765, 0.95105654, 0.96858317, 0.9822873, 0.9921147, 0.9980267, 1, 0.9980267, 0.99211466, 0.9822872, 0.96858317, 0.95105654, 0.9297765, 0.904827, 0.8763066, 0.84432787, 0.8090169, 0.77051324, 0.7289686, 0.68454707, 0.63742393, 0.5877852, 0.5358269, 0.48175356, 0.42577937, 0.3681244, 0.30901703, 0.2486899, 0.1873813, 0.1253332, 0.06279046, -0.00000008742278, -0.06279063, -0.12533337, -0.18738124, -0.24869007, -0.30901697, -0.36812478, -0.4257793, -0.4817535, -0.53582686, -0.5877851, -0.6374241, -0.68454707, -0.72896874, -0.77051336, -0.8090168, -0.84432805, -0.8763066, -0.9048272, -0.92977643, -0.9510566, -0.96858317, -0.9822872, -0.9921147, -0.9980267, -1, -0.9980267, -0.9921147, -0.9822872, -0.9685831, -0.9510565, -0.92977643, -0.9048272, -0.8763066, -0.8443278, -0.8090171, -0.77051336, -0.7289684, -0.6845468, -0.63742405, -0.5877853, -0.53582644, -0.4817537, -0.42577928, -0.36812454, -0.3090165, -0.24868982, -0.18738122, -0.12533312, -0.06279085, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.06279052, 0.12533323, 0.18738131, 0.2486899, 0.309017, 0.36812454, 0.4257793, 0.4817537, 0.5358268, 0.58778524, 0.63742405, 0.6845471, 0.7289687, 0.7705133, 0.809017, 0.8443279, 0.8763067, 0.90482706, 0.9297765, 0.95105654, 0.96858317, 0.9822873, 0.9921147, 0.9980267, 1, 0.9980267, 0.99211466, 0.9822872, 0.96858317, 0.95105654, 0.9297765, 0.904827, 0.8763066, 0.84432787, 0.8090169, 0.77051324, 0.7289686, 0.68454707, 0.63742393, 0.5877852, 0.5358269, 0.48175356, 0.42577937, 0.3681244, 0.30901703, 0.2486899, 0.1873813, 0.1253332, 0.06279046, -0.00000008742278, -0.06279063, -0.12533337, -0.18738124, -0.24869007, -0.30901697, -0.36812478, -0.4257793, -0.4817535, -0.53582686, -0.5877851, -0.6374241, -0.68454707, -0.72896874, -0.77051336, -0.8090168, -0.84432805, -0.8763066, -0.9048272, -0.92977643, -0.9510566, -0.96858317, -0.9822872, -0.9921147, -0.9980267, -1, -0.9980267, -0.9921147, -0.9822872, -0.9685831, -0.9510565, -0.92977643, -0.9048272, -0.8763066, -0.8443278, -0.8090171, -0.77051336, -0.7289684, -0.6845468, -0.63742405, -0.5877853, -0.53582644, -0.4817537, -0.42577928, -0.36812454, -0.3090165, -0.24868982, -0.18738122, -0.12533312, -0.06279085, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ]
plt.figure()
plt.title("v3")
plt.plot(v3)
v1 = [ 0, 0.06279052, 0.12533323, 0.18738131, 0.2486899, 0.309017, 0.36812454, 0.4257793, 0.4817537, 0.5358268, 0.58778524, 0.63742405, 0.6845471, 0.7289687, 0.7705133, 0.809017, 0.8443279, 0.8763067, 0.90482706, 0.9297765, 0.95105654, 0.96858317, 0.9822873, 0.9921147, 0.9980267, 1, 0.9980267, 0.99211466, 0.9822872, 0.96858317, 0.95105654, 0.9297765, 0.904827, 0.8763066, 0.84432787, 0.8090169, 0.77051324, 0.7289686, 0.68454707, 0.63742393, 0.5877852, 0.5358269, 0.48175356, 0.42577937, 0.3681244, 0.30901703, 0.2486899, 0.1873813, 0.1253332, 0.06279046, -0.00000008742278, -0.06279063, -0.12533337, -0.18738124, -0.24869007, -0.30901697, -0.36812478, -0.4257793, -0.4817535, -0.53582686, -0.5877851, -0.6374241, -0.68454707, -0.72896874, -0.77051336, -0.8090168, -0.84432805, -0.8763066, -0.9048272, -0.92977643, -0.9510566, -0.96858317, -0.9822872, -0.9921147, -0.9980267, -1, -0.9980267, -0.9921147, -0.9822872, -0.9685831, -0.9510565, -0.92977643, -0.9048272, -0.8763066, -0.8443278, -0.8090171, -0.77051336, -0.7289684, -0.6845468, -0.63742405, -0.5877853, -0.53582644, -0.4817537, -0.42577928, -0.36812454, -0.3090165, -0.24868982, -0.18738122, -0.12533312, -0.06279085, ]
plt.figure()
plt.title("v1")
plt.plot(v1)

plt.show()
