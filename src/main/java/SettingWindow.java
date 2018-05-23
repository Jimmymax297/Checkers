import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import static java.lang.System.exit;

public class SettingWindow extends Frame {
    public enum ButtonColour {White, Black}

    public SettingWindow(){
        setWindow();
        setVisible(true);
        Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        setLocation(dim.width/2-this.getSize().width/2, dim.height/2-this.getSize().height/2);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent windowEvent){
                exit(0);
            }
        });
    }

    private void setWindow(){

        setTitle("Checkers");

        Label text = new Label();
        text.setText("Choose colour");
        text.setAlignment(Label.CENTER);

        Button whiteButton = new Button();
        whiteButton.setLabel("White");
        whiteButton.addActionListener(click ->{
            setEnabled(false);
            Object board = new BoardWindow(ButtonColour.White, this);
        });

        Button blackButton = new Button();
        blackButton.setLabel("Black");
        blackButton.addActionListener(click ->{
            setEnabled(false);
            Object board = new BoardWindow(ButtonColour.Black, this );
        });

        Label space = new Label();

        Button exitButton = new Button();
        exitButton.setLabel("Exit");
        exitButton.addActionListener(click -> {exit(0);});

        Panel panel = new Panel();
        panel.setLayout(new GridLayout(5,1));
        panel.add(text);
        panel.add(whiteButton);
        panel.add(blackButton);
        panel.add(space);
        panel.add(exitButton);
        add(panel);
        setSize(400,200);
        setResizable(false);
    }


    public static void main(String[] Args) {
        SettingWindow win = new SettingWindow();
        System.out.println("Window is working");
    }
}
