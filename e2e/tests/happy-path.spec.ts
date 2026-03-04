import { test, expect } from '../fixtures/containerFixture';

test.describe('Lain-Emacs Happy Path', () => {

    test('should navigate from base to scrum and view tasks sequentially', async ({ basePage, scrumPage, taskViewPage }) => {
        // Step 2.1: Navigation and Authentication
        await basePage.goto();
        await basePage.authenticate();

        // Step 2.2: Access the Agenda
        await scrumPage.accessScrumView();
        await expect(scrumPage.page).toHaveURL(/SCRUM\.html/);

        // Step 2.3: Validate Task 1 (eiusmod)
        await scrumPage.clickTask('eiusmod');
        await taskViewPage.verifyTaskContent('eiusmod');
        await taskViewPage.verifyTaskContent('Check task'); // Button text

        // Step 2.4: Validate Task 2 (labore) - Sequential State Check
        await scrumPage.goto();
        await scrumPage.clickTask('labore');

        // Crucially check that eiusmod heading is NOT here anymore (Narrowing Reset)
        await taskViewPage.verifyTaskContent('labore');
        const bodyText = await taskViewPage.page.textContent('body');
        // We check for the Org heading format to be sure we are checking the task itself
        // and not some random occurrence in description.
        expect(bodyText).not.toContain('*** TODO eiusmod');
    });

    test('should handle tasks with special characters', async ({ basePage, scrumPage, taskViewPage }) => {
        await basePage.goto();
        await basePage.authenticate();
        await scrumPage.accessScrumView();

        // Testing the task with special characters added to scrum.org
        const specialTaskName = 'Task [with] special? characters';
        await scrumPage.clickTask(specialTaskName);
        await taskViewPage.verifyTaskContent(specialTaskName);
    });
});
