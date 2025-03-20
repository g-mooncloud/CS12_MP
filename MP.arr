use context essentials2021
### CS 12 21.2 MP JUST JUMP (Simple Egg Toss Clone) ###

### PURISIMA, ANN GABRIELLE C. - 2021-10796, Role: Leadership, Tester, Code Review ###
### RAMOS, NICO SEBASTIAN B. - 2021-11818, Role: Leadership, Tester, Code Review #####


include image
include reactors
############### DATA AND TYPES ###############
data PlatformLevel:
  | top
  | middle
  | bottom
end

data GameStatus:
  | ongoing
  | transitioning
  | game-over
end

type Platform = {
  x :: Number,
  y :: Number,
  dx :: Number,
}

type Egg = {
  x :: Number,
  y :: Number,
  dx :: Number,
  dy :: Number,
  ay :: Number,
  is-airborne :: Boolean,
}

type State = {
  game-status :: GameStatus,
  egg :: Egg,
  top-platform :: Platform,
  middle-platform :: Platform,
  bottom-platform :: Platform,
  current-platform :: PlatformLevel,
  other-platforms :: List<Platform>,
  score :: Number,
  lives :: Number,
}


############### SETTINGS ###############
FPS = 60
screen-width = 300
screen-height = 500
world-bgc = 'light-blue'
pan-speed = 5

egg-color = 'fire-brick'
egg-radius = 15
egg-lives = 12
egg-jump-height = -12

platform-color = 'royal-blue'
platform-width = 50
platform-height = 15 / 2
platform-default-x = screen-width / 2
platform-default-y-top = screen-height / 4
platform-default-y-middle = screen-height / 2
platform-default-y-bottom = (screen-height * 3) / 4
platform-maximum-speed = 50

############### QUALITY OF LIFE ###############
fun velocity(speed :: Number) -> Number:
  boolean = 0.5 > num-random(2)
  if boolean:
    0.1 * (-1 * ((num-random(speed)) + 1))
  else:
    0.1 * (num-random(speed) + 1)
  end
end

fun generate-platform(level-y :: Number):
  {x: platform-default-x, y: level-y, dx: velocity(platform-maximum-speed)}
end

fun get-current-platform(state :: State) -> Platform:
  cases (PlatformLevel) state.current-platform:
    | top    => state.top-platform
    | middle => state.middle-platform
    | bottom => state.bottom-platform
  end
end

############### INITIAL TYPE ####################
initial-bottom-platform = {
  x: platform-default-x,
  y: platform-default-y-bottom,
  dx: velocity(platform-maximum-speed),
}

initial-egg = {
  x: generate-platform(platform-default-y-bottom).x + (platform-width / 2),
  y: generate-platform(platform-default-y-bottom).y - egg-radius,
  dx: generate-platform(platform-default-y-bottom).dx,
  dy: 0,
  ay: 0.4,
  is-airborne: false,
}

initial-state = {
  game-status: ongoing,
  egg: initial-egg,
  top-platform: generate-platform(platform-default-y-top),
  middle-platform: generate-platform(platform-default-y-middle),
  bottom-platform: generate-platform(platform-default-y-bottom),
  current-platform: bottom,
  other-platforms: empty,
  score: 0,
  lives: egg-lives,
}

############### DRAW WORLD ###############
fun draw-egg(state :: State, img :: Image) -> Image:
  egg = circle(egg-radius, 'solid', egg-color)
  place-image(egg, state.egg.x, state.egg.y, img)
end

fun draw-score(state :: State, img :: Image) -> Image:
  text-scoreboard = text(num-to-string(state.score), 24, "black")
  place-image(text-scoreboard, screen-width / 2, screen-height / 10, img)
end

fun draw-lives(state :: State, img :: Image) -> Image:
  text-lives-count = text(("Lives: " + num-to-string(state.lives)), 18, "black")
  place-image(text-lives-count, screen-width - 40, 20, img)
end

fun draw-game-over(state :: State, img :: Image) -> Image:
  if state.game-status == game-over:
    text-img = text("GAME OVER", 48, "red")
    place-image(text-img, screen-width / 2, screen-height / 2, img)
  else:
    img
  end
end

fun draw-single-platform(platform :: Platform, img :: Image) -> Image:
  new-platform = rectangle(platform-width, platform-height, "solid", platform-color)  
  place-image-align(new-platform, platform.x, platform.y, "left", "top", img)
end

fun draw-other-platforms(state :: State, img :: Image) -> Image:
  img
    ^ draw-single-platform(state.top-platform, _)
    ^ draw-single-platform(state.middle-platform, _)
    ^ draw-single-platform(state.bottom-platform, _)
end

fun draw-new-platform(state :: State, img :: Image) -> Image:
  if state.game-status == transitioning:
    img
      ^ draw-single-platform(state.other-platforms.get(0), _)
      ^ draw-single-platform(state.other-platforms.get(1), _)
  else:
    img
  end
end

fun draw-world(state :: State) -> Image:
  window = empty-color-scene(screen-width, screen-height, world-bgc)
  window
    ^ draw-lives(state, _)
    ^ draw-other-platforms(state, _)
    ^ draw-new-platform(state, _)
    ^ draw-egg(state, _)
    ^ draw-score(state, _)
    ^ draw-game-over(state, _)
end

############### KEYBOARD ####################
fun hotkeys(state :: State, key :: String) -> State:
  if key == ' ':
    cases (GameStatus) state.game-status:
      | ongoing => 
        if state.egg.is-airborne:
          state
        else:
          state.{egg: state.egg.{dy: state.egg.dy + egg-jump-height, is-airborne: true}}
        end
      | transitioning => state
      | game-over     => initial-state
    end
  else:
    state
  end
end

############### TICKS ####################
#####---------- OBJECT PHYSICS ----------#####
#EGG PHYSICS
fun update-egg-velocity-y(state :: State) -> State:
  state.{egg: (state.egg.{dy: state.egg.dy + state.egg.ay})}
end

fun update-egg-position-y(state :: State) -> State:
  state.{egg: (state.egg.{y: state.egg.y + state.egg.dy})}
end

fun update-egg-x(state :: State) -> State:
  match-platform-speed = get-current-platform(state).dx
  state.{egg: (state.egg.{x: state.egg.x + match-platform-speed})}
end

fun update-egg-y(state :: State) -> State:
    state
      ^ update-egg-velocity-y(_)
      ^ update-egg-position-y(_)
end

fun update-egg(state :: State) -> State:
  if state.egg.is-airborne:
    state
      ^ update-egg-y
  else:
    state
      ^ update-egg-x
  end
end

#PLATFORM PHYSICS
fun update-platform-movement(platform :: Platform) -> Platform:
  if (platform.x <= 0) or ((platform-width + platform.x) >= screen-width):   
    platform.{dx: -1 * platform.dx, 
      x: if platform.x <= 0:
          1
        else:
          screen-width - platform-width - 1
      end
    }
  else:
    platform.{x: platform.x + platform.dx}
  end
end
  
fun updated-platforms(state :: State) -> State:
  updated-top = update-platform-movement(state.top-platform)
  updated-middle = update-platform-movement(state.middle-platform)
  updated-bottom = update-platform-movement(state.bottom-platform)
  state.{top-platform: updated-top, middle-platform: updated-middle, bottom-platform: updated-bottom}
end

#####---------- OBJECTIVE STATUS ----------#####
#EGG LANDING ON PLATFORM
fun get-next-jump(state :: State):
  cases (PlatformLevel) state.current-platform:
    | top    => state.bottom-platform
    | middle => state.top-platform
    | bottom => state.middle-platform
  end
end

fun landing-status(state :: State) -> State:
  egg-footing-x = state.egg.x
  egg-footing-y = state.egg.y + egg-radius
  
  next-platform-x = get-next-jump(state).x
  next-platform-y = get-next-jump(state).y
  next-platform-dx = get-next-jump(state).dx
  
  within-platform-x = (egg-footing-x >= next-platform-x) and (egg-footing-x <= (next-platform-x + platform-width))
  within-platform-y = (egg-footing-y >= next-platform-y) and (egg-footing-y <= (next-platform-y + platform-height))
  falling-state = state.egg.dy >= 0

  if within-platform-x and within-platform-y and falling-state:
    updated-egg = state.egg.{x: egg-footing-x, y: next-platform-y - egg-radius, dx: next-platform-dx, dy: 0, is-airborne: false}
    new-platform =
      cases (PlatformLevel) state.current-platform:
        | top => bottom
        | middle => top
        | bottom => middle
      end
    check-game-status =
      if new-platform == top:
        transitioning
      else:
        state.game-status
      end
    change-score =
      if (state.current-platform == bottom) or (state.current-platform == middle):
        state.score + 1
      else:
        state.score
      end
    state.{egg: updated-egg, current-platform: new-platform}.{score: change-score}. {game-status: check-game-status}
  else:
    state
  end
end

#RESPAWN MECHANIC
fun update-lives(state :: State) -> State:
  egg-bottom = state.egg.y + egg-radius
  fell = egg-bottom >= screen-height
  
  checkpoint-platform-x = get-current-platform(state).x
  checkpoint-platform-y = get-current-platform(state).y
  checkpoint-platform-dx = get-current-platform(state).dx
  checkpoint = state.egg.{x: checkpoint-platform-x + (platform-width / 2), y: checkpoint-platform-y - egg-radius, dx: checkpoint-platform-dx, dy: 0, is-airborne: false}
  
  if state.lives > 0:
    if fell:
      state.{lives: state.lives - 1, egg: checkpoint}
    else:
      state
    end
  else:
    state.{game-status: game-over}.{lives: 0, egg: checkpoint}
  end
end

#GENERATE NEW PLATFORMS
fun update-next-platforms(state :: State) -> State:
  fun generate-new-platforms() -> List<Platform>:
    new-middle-platform-y = (-1 * platform-default-y-top) + 125 
    new-top-platform-y = (-1 * platform-default-y-middle) + 125

    new-middle-platform = generate-platform(new-middle-platform-y)
    new-top-platform = generate-platform(new-top-platform-y)

    [list: new-middle-platform, new-top-platform]
  end
  if state.current-platform == top:
    state.{other-platforms: generate-new-platforms()}
  else:
    state
  end
end

fun update-next-stage(state :: State) -> State:
  if state.top-platform.y < platform-default-y-bottom:
    current-bottom = state.bottom-platform.{y: state.bottom-platform.y + pan-speed}
    current-middle = state.middle-platform.{y: state.middle-platform.y + pan-speed}
    current-top = state.top-platform.{y: state.top-platform.y + pan-speed}
    updated-egg = state.egg.{y: state.egg.y + pan-speed}
    upcoming-middle = state.other-platforms.get(0).{y: state.other-platforms.get(0).y + pan-speed}
    upcoming-top = state.other-platforms.get(1).{y: state.other-platforms.get(1).y + pan-speed}
    state.{bottom-platform: current-bottom, middle-platform: current-middle, top-platform: current-top, egg: updated-egg, other-platforms: [list: upcoming-middle, upcoming-top]}
  else:
    state.{game-status: ongoing}.{bottom-platform: state.top-platform}.{middle-platform: state.other-platforms.get(0)}.{top-platform: state.other-platforms.get(1)}.{other-platforms: [list: ]}
  end
end

fun ticks-set(state :: State) -> State:
  cases (GameStatus) state.game-status:
    | game-over =>
      state
    | transitioning =>
      state
        ^ update-next-stage(_)
    | ongoing =>
      state
        ^ updated-platforms(_)
        ^ update-egg(_)
        ^ landing-status(_)
        ^ update-lives(_)
        ^ update-next-platforms(_)
  end
end

stage = reactor:
  title: 'CS 12 21.2 MP Just Jump! (Simple Egg Toss Clone)',
  init: initial-state,
  to-draw: draw-world,
  seconds-per-tick: 1 / FPS,
  on-key: hotkeys,
  on-tick: ticks-set,
end

interact(stage)

